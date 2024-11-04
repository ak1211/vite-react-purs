/*
 * LVGL_M5ENV_SQLITE.ino
 * SPDX-FileCopyrightText: 2024 Yamamoto Akihiro  <https://github.com/ak1211>
 * SPDX-License-Identifier: MIT
 */
#include <cassert>
#include <ctime>

#include <ezTime.h>
#include <lvgl.h>
#include <sqlite3.h>
#include <M5Unified.h>
#include <M5UnitENV.h>
#include <SD.h>

/* 日本語表示用フォント */
const lv_font_t *FONT_JA_JP{ &lv_font_simsun_16_cjk };

/* LVGLバッファに割り当てるメモリは 16kib くらいにしておく */
constexpr size_t DRAW_BUF_SIZE{ 16 * 1024 };

/* データーベースプラグマ */
constexpr char *DATABASE_PRAGMA[] = {
  "PRAGMA auto_vacuum = FULL;",
  "PRAGMA temp_store = MEMORY;",
  /* データーベースのファイルサイズは page_count * page_size */
  "PRAGMA page_size = 1024;",      /* データーベースのページサイズ */
  "PRAGMA max_page_count = 1024;", /* データーベースの最大ページ数 */
  "vacuum;",
};

/* センサーID */
typedef uint64_t sensor_id_t;
#define TO_SENSOR_ID(_h, _g, _f, _e, _d, _c, _b, _a) ((sensor_id_t)(_a) | (sensor_id_t)(_b) << 8 | (sensor_id_t)(_c) << 16 | (sensor_id_t)(_d) << 24 | (sensor_id_t)(_e) << 32 | (sensor_id_t)(_f) << 40 | (sensor_id_t)(_g) << 48 | (sensor_id_t)(_h) << 56)
constexpr static sensor_id_t SENSOR_ID_SHT30 = TO_SENSOR_ID('S', 'H', 'T', '3', '0', '\0', '\0', '\0');
constexpr static sensor_id_t SENSOR_ID_BMP280 = TO_SENSOR_ID('B', 'M', 'P', '2', '8', '0', '\0', '\0');

/* 温度テーブル */
constexpr char *SQL_CRETE_TABLE_TEMPERATURE{
  "CREATE TABLE IF NOT EXISTS temperature"
  "(id INTEGER PRIMARY KEY AUTOINCREMENT" /* KEY */
  ",sensor_id INTEGER NOT NULL"           /* センサー */
  ",location TEXT"                        /* 場所(ヌル許容) */
  ",at INTEGER NOT NULL"                  /* 時間(unix time) */
  ",milli_degc INTEGER NOT NULL"          /* 温度(℃)の1/1000*/
  ");"
};
/* 湿度テーブル */
constexpr char *SQL_CREATE_TABLE_RELATIVE_HUMIDITY{
  "CREATE TABLE IF NOT EXISTS relative_humidity"
  "(id INTEGER PRIMARY KEY AUTOINCREMENT" /* KEY */
  ",sensor_id INTEGER NOT NULL"           /* センサー */
  ",location TEXT"                        /* 場所(ヌル許容) */
  ",at INTEGER NOT NULL"                  /* 時間(unix time) */
  ",ppm_rh INTEGER NOT NULL"              /* 湿度の1/1000,000(百万分の１) */
  ");"
};
/* 気圧テーブル */
constexpr char *SQL_CREATE_TABLE_PRESSURE{
  "CREATE TABLE IF NOT EXISTS pressure"
  "(id INTEGER PRIMARY KEY AUTOINCREMENT" /* KEY */
  ",sensor_id INTEGER NOT NULL"           /* センサー */
  ",location TEXT"                        /* 場所(ヌル許容) */
  ",at INTEGER NOT NULL"                  /* 時間(unix time) */
  ",pascal INTEGER NOT NULL"              /* 気圧(Pa) */
  ");"
};
/* データーベーステーブル */
constexpr char *DATABASE_SQL_CREATE_TABLE[] = {
  SQL_CRETE_TABLE_TEMPERATURE, SQL_CREATE_TABLE_RELATIVE_HUMIDITY, SQL_CREATE_TABLE_PRESSURE
};

/*
 * 前方宣言
 */
class M5UnitEnv2;
class EnvDatabaseConnection;
uint32_t my_tick();
void my_print(lv_log_level_t level, const char *buf);
void my_disp_flush(lv_display_t *disp, const lv_area_t *area, uint8_t *px_map);
void my_touchpad_read(lv_indev_t *indev, lv_indev_data_t *data);
void show_fatal_alert(const String &message);
void restore_datetime_from_RTC();
void vibrate(uint16_t millisec);
void system_restart();
lv_obj_t *create_screen_default();

/*
 * sqlite3用
 */
const sqlite3_mem_methods DATABASE_CUSTOM_MEM_METHODS{
  /* メモリー割り当て関数 */
  .xMalloc = [](int size) -> void * {
    return heap_caps_aligned_alloc(8, size,
                                   MALLOC_CAP_8BIT | MALLOC_CAP_SPIRAM);
  },
  /* メモリー解放関数 */
  .xFree = heap_caps_free,
  /* メモリー再割り当て関数 */
  .xRealloc = [](void *ptr, int size) -> void * {
    return heap_caps_realloc(ptr, size, MALLOC_CAP_8BIT | MALLOC_CAP_SPIRAM);
  },
  /* 割り当てサイズを返す関数 */
  .xSize = [](void *ptr) -> int {
    return heap_caps_get_allocated_size(ptr);
  },
  /* 8の倍数に切り上げる関数 */
  .xRoundup = [](int size) -> int {
    return (size + 7) & ~7;
  },
  /* メモリー割り当て初期化 */
  .xInit = [](void *app_data) -> int {
    return 0;  // nothing to do
  },
  /* メモリー割り当て後片付け */
  .xShutdown = [](void *app_data) -> void {
    // nothing to do
  },
  /* xInit() と xShutdown()の引数 */
  .pAppData = nullptr,
};

/*
 * グローバル変数定義
 */
struct GlobalVar {
  GlobalVar() {
    int rc;
    /* sqlite3初期化 */
    if ((rc = sqlite3_initialize()) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
    }
    if (psramFound()) {
      //
      M5_LOGI("Database uses heap on SPIRAM");
      if (auto result = sqlite3_config(
            SQLITE_CONFIG_MALLOC,
            static_cast<const sqlite3_mem_methods *>(&DATABASE_CUSTOM_MEM_METHODS));
          result != SQLITE_OK) {
        M5_LOGE("sqlite3_config() failure: %d", result);
      }
    }
  }
  ~GlobalVar() {
    delete envDatabaseConnection;
    delete m5unit_env2;
    int rc;
    /* sqlite3終了 */
    if ((rc = sqlite3_shutdown()) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
    }
  }
  /* pushImageDMAの都合上 LVGLが使うバッファはヒープまたはスタック領域には配置しない */
  uint32_t _lvgl_draw_buf[DRAW_BUF_SIZE / sizeof(uint32_t)];
  /* ezTime */
  Timezone tz_local;
  /* センサー */
  M5UnitEnv2 *m5unit_env2;
  /* データーベース */
  EnvDatabaseConnection *envDatabaseConnection{ nullptr };
  /* タブビュー */
  lv_obj_t *displayed_tabview{ nullptr };
};
static GlobalVar global_var{};

/* 環境データーベース接続 */
class EnvDatabaseConnection {
  sqlite3 *_connection;
  EnvDatabaseConnection()
    : _connection{ nullptr } {}
public:
  ~EnvDatabaseConnection() {
    if (_connection) {
      sqlite3_close(_connection);
    }
  }
  /* データーベースを開く */
  static EnvDatabaseConnection *open_connection(const char *open_to_file_path) {
    char *error_message{ nullptr };
    int rc{ SQLITE_OK };
    /* インスタンスを用意する */
    auto instance = new EnvDatabaseConnection();
    if (!instance) {
      M5_LOGE("memory allocation error");
      goto error_exit;
    }
    /* データーベースファイルオープン */
    assert(instance);
    if ((rc = sqlite3_open_v2(
           open_to_file_path, &instance->_connection,
           SQLITE_OPEN_CREATE | SQLITE_OPEN_READWRITE | SQLITE_OPEN_URI,
           nullptr))
        != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* データーベースプラグマを発行する */
    assert(instance->_connection);
    for (auto idx = 0; idx < sizeof(DATABASE_PRAGMA) / sizeof(DATABASE_PRAGMA[0]); ++idx) {
      M5_LOGD("exec: \"%s\"", DATABASE_PRAGMA[idx]);
      assert(!error_message);
      if ((rc = sqlite3_exec(instance->_connection, DATABASE_PRAGMA[idx], nullptr, nullptr, &error_message)) != SQLITE_OK) {
        M5_LOGE("sqlite3 error: %d", rc);
        goto error_exit;
      }
    }
    /* 成功 */
    return instance;
error_exit:
    if (error_message) {
      M5_LOGE("%s", error_message);
    }
    sqlite3_free(error_message);
    if (instance) {
      M5_LOGE("%s", sqlite3_errmsg(instance->_connection));
    }
    delete instance;
    return nullptr;
  }
  /* 温度テーブルに挿入する */
  bool insert_temperature(uint64_t sensor_id, char *location, time_t at, int32_t milli_degc) {
    constexpr char *query{ "INSERT INTO temperature(sensor_id,location,at,milli_degc) VALUES(?,?,?,?);" };
    sqlite3_stmt *stmt{ nullptr };
    int rc{ SQLITE_OK };

    if (!_connection) {
      M5_LOGE("No connection");
      goto error_exit;
    }
    /* プリペアドステートメント */
    if ((rc = sqlite3_prepare_v2(_connection, query, -1, &stmt,
                                 nullptr))
        != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /*
     * 値の設定
    */
    assert(stmt);
    /* センサー */
    if ((rc = sqlite3_bind_int64(stmt, 1, sensor_id)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 場所(ヌル許容) */
    if ((rc = sqlite3_bind_text(stmt, 2, location, -1, SQLITE_STATIC)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 時間(unix time) */
    if ((rc = sqlite3_bind_int64(stmt, 3, static_cast<int64_t>(at))) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 温度(℃)の1/1000 */
    if ((rc = sqlite3_bind_int(stmt, 4, milli_degc)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* SQL表示(デバッグ用) */
    if (CORE_DEBUG_LEVEL >= ESP_LOG_DEBUG) {
      char *p = sqlite3_expanded_sql(stmt);
      if (p) {
        M5_LOGD("%s", p);
      }
      sqlite3_free(p);
    }
    /* */
    while (true) {
      rc = sqlite3_step(stmt);
      if (rc == SQLITE_ROW) {
        continue;
      } else if (rc == SQLITE_DONE) {
        break;
      } else if (rc == SQLITE_OK) {
        break;
      } else {
        M5_LOGE("sqlite3 error: %d", rc);
        goto error_exit;
      }
    }
    /* */
    if ((rc = sqlite3_finalize(stmt)) != SQLITE_OK) {
      stmt = nullptr;
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 成功 */
    return true;

error_exit:
    sqlite3_finalize(stmt);
    if (_connection) {
      M5_LOGE("%s", sqlite3_errmsg(_connection));
    }
    return false;
  }
  /* 湿度テーブルに挿入する */
  bool insert_relative_humidity(uint64_t sensor_id, char *location, time_t at, int32_t ppm_rh) {
    constexpr char *query{ "INSERT INTO relative_humidity(sensor_id,location,at,ppm_rh) VALUES(?,?,?,?);" };
    sqlite3_stmt *stmt{ nullptr };
    int rc{ SQLITE_OK };

    if (!_connection) {
      M5_LOGE("No connection");
      goto error_exit;
    }
    /* プリペアドステートメント */
    if ((rc = sqlite3_prepare_v2(_connection, query, -1, &stmt,
                                 nullptr))
        != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /*
     * 値の設定
    */
    assert(stmt);
    /* センサー */
    if ((rc = sqlite3_bind_int64(stmt, 1, sensor_id)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 場所(ヌル許容) */
    if ((rc = sqlite3_bind_text(stmt, 2, location, -1, SQLITE_STATIC)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 時間(unix time) */
    if ((rc = sqlite3_bind_int64(stmt, 3, static_cast<int64_t>(at))) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 湿度の1/1000,000(百万分の１) */
    if ((rc = sqlite3_bind_int(stmt, 4, ppm_rh)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* SQL表示(デバッグ用) */
    if (CORE_DEBUG_LEVEL >= ESP_LOG_DEBUG) {
      char *p = sqlite3_expanded_sql(stmt);
      if (p) {
        M5_LOGD("%s", p);
      }
      sqlite3_free(p);
    }
    /* */
    while (true) {
      rc = sqlite3_step(stmt);
      if (rc == SQLITE_ROW) {
        continue;
      } else if (rc == SQLITE_DONE) {
        break;
      } else if (rc == SQLITE_OK) {
        break;
      } else {
        M5_LOGE("sqlite3 error: %d", rc);
        goto error_exit;
      }
    }
    /* */
    if ((rc = sqlite3_finalize(stmt)) != SQLITE_OK) {
      stmt = nullptr;
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 成功 */
    return true;

error_exit:
    sqlite3_finalize(stmt);
    if (_connection) {
      M5_LOGE("%s", sqlite3_errmsg(_connection));
    }
    return false;
  }
  /* 気圧テーブルに挿入する */
  bool insert_pressure(uint64_t sensor_id, char *location, time_t at, int32_t pascal) {
    constexpr char *query{ "INSERT INTO pressure(sensor_id,location,at,pascal) VALUES(?,?,?,?);" };
    sqlite3_stmt *stmt{ nullptr };
    int rc{ SQLITE_OK };

    if (!_connection) {
      M5_LOGE("No connection");
      goto error_exit;
    }
    /* プリペアドステートメント */
    if ((rc = sqlite3_prepare_v2(_connection, query, -1, &stmt,
                                 nullptr))
        != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /*
     * 値の設定
    */
    assert(stmt);
    /* センサー */
    if ((rc = sqlite3_bind_int64(stmt, 1, sensor_id)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 場所(ヌル許容) */
    if ((rc = sqlite3_bind_text(stmt, 2, location, -1, SQLITE_STATIC)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 時間(unix time) */
    if ((rc = sqlite3_bind_int64(stmt, 3, static_cast<int64_t>(at))) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 気圧(Pa) */
    if ((rc = sqlite3_bind_int(stmt, 4, pascal)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* SQL表示(デバッグ用) */
    if (CORE_DEBUG_LEVEL >= ESP_LOG_DEBUG) {
      char *p = sqlite3_expanded_sql(stmt);
      if (p) {
        M5_LOGD("%s", p);
      }
      sqlite3_free(p);
    }
    /* */
    while (true) {
      rc = sqlite3_step(stmt);
      if (rc == SQLITE_ROW) {
        continue;
      } else if (rc == SQLITE_DONE) {
        break;
      } else if (rc == SQLITE_OK) {
        break;
      } else {
        M5_LOGE("sqlite3 error: %d", rc);
        goto error_exit;
      }
    }
    /* */
    if ((rc = sqlite3_finalize(stmt)) != SQLITE_OK) {
      stmt = nullptr;
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 成功 */
    return true;

error_exit:
    sqlite3_finalize(stmt);
    if (_connection) {
      M5_LOGE("%s", sqlite3_errmsg(_connection));
    }
    return false;
  }
  /* テーブルを作成する */
  bool
  create_all_tables() {
    int rc{ SQLITE_OK };
    char *error_message{ nullptr };
    if (!_connection) {
      M5_LOGE("No connection");
      goto error_exit;
    }
    /* テーブルを作成する */
    assert(_connection);
    for (auto idx = 0; idx < sizeof(DATABASE_SQL_CREATE_TABLE) / sizeof(DATABASE_SQL_CREATE_TABLE[0]); ++idx) {
      assert(!error_message);
      if ((rc =
             sqlite3_exec(_connection, DATABASE_SQL_CREATE_TABLE[idx], nullptr, nullptr,
                          &error_message))
          != SQLITE_OK) {
        M5_LOGD("%s", DATABASE_SQL_CREATE_TABLE[idx]);
        M5_LOGE("sqlite3 error: %d", rc);
        goto error_exit;
      }
    }
    /* 成功 */
    return true;
error_exit:
    if (error_message) {
      M5_LOGE("%s", error_message);
    }
    sqlite3_free(error_message);
    return false;
  }
  /* ファイル保存する */
  bool save_to_file(const char *save_to_file_path) {
    sqlite3 *new_db_connection{ nullptr };
    sqlite3_backup *backup{ nullptr };
    int rc{ SQLITE_OK };
    if (!_connection) {
      M5_LOGE("No connection");
      goto error_exit;
    }

    /* SDカード */
    while (SD.begin(GPIO_NUM_4, SPI, 25000000) == false) {
      delay(500);
    }

    /* 保存対象のデーターベースファイルを開く */
    if ((rc = sqlite3_open_v2(
           save_to_file_path, &new_db_connection,
           SQLITE_OPEN_CREATE | SQLITE_OPEN_READWRITE | SQLITE_OPEN_URI,
           nullptr))
        != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    assert(new_db_connection);
    /* バックアップ始まり */
    backup = sqlite3_backup_init(new_db_connection, "main", _connection, "main");
    if (backup) {
      /* バックアップループ */
      while (true) {
        rc = sqlite3_backup_step(backup, -1);
        if (rc == SQLITE_OK) {
          continue;
        } else if (rc == SQLITE_DONE) {
          break;
        } else {
          M5_LOGE("sqlite3 error: %d", rc);
          goto error_exit;
        }
      }
    }
    /* バックアップ終わり */
    if ((rc = sqlite3_backup_finish(backup)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    backup = nullptr;
    /* 閉じる */
    if ((rc = sqlite3_close(new_db_connection)) != SQLITE_OK) {
      M5_LOGE("sqlite3 error: %d", rc);
      goto error_exit;
    }
    /* 成功 */
    return true;

error_exit:
    if (new_db_connection) {
      M5_LOGE("%s", sqlite3_errmsg(new_db_connection));
    }
    sqlite3_backup_finish(backup);
    sqlite3_close(new_db_connection);
    return false;
  }
};

/* M5Unit-ENV2センサー */
class M5UnitEnv2 {
  M5UnitEnv2() {}
public:
  SHT3X sht3x;
  BMP280 bmp;
  /* M5Unit ENV2を初期化する */
  static M5UnitEnv2 *begin() {
    auto instance = new M5UnitEnv2();
    if (instance == nullptr) {
      M5_LOGE("memory allocation error");
      goto error;
    }
    assert(instance);
    if (!instance->sht3x.begin(&Wire, SHT3X_I2C_ADDR, M5.Ex_I2C.getSDA(), M5.Ex_I2C.getSCL(), 400000U)) {
      /* SHT3XがI2Cバス上に見つからない */
      M5_LOGE("Couldn't find SHT3X");
      goto error;
    }
    if (!instance->bmp.begin(&Wire, BMP280_I2C_ADDR, M5.Ex_I2C.getSDA(), M5.Ex_I2C.getSCL(), 400000U)) {
      /* BMP280がI2Cバス上に見つからない */
      M5_LOGE("Couldn't find BMP280");
      goto error;
    }
    /* BMP280は規定値に設定する */
    instance->bmp.setSampling(BMP280::MODE_NORMAL,     /* Operating Mode. */
                              BMP280::SAMPLING_X2,     /* Temp. oversampling */
                              BMP280::SAMPLING_X16,    /* Pressure oversampling */
                              BMP280::FILTER_X16,      /* Filtering. */
                              BMP280::STANDBY_MS_500); /* Standby time. */
                                                       /* 成功 */
    return instance;

error:
    delete instance;
    return nullptr;
  }
  /* 環境測定 */
  void update() {
    time_t utc = UTC.now();

    if (sht3x.update()) {
      /*
      Serial.println("-----SHT3X-----");
      Serial.print("Temperature: ");
      Serial.print(sht3x.cTemp);
      Serial.println(" degrees C");
      Serial.print("Humidity: ");
      Serial.print(sht3x.humidity);
      Serial.println("% rH");
      Serial.println("-------------\r\n");
      */
      /* データーベースに挿入する */
      assert(global_var.envDatabaseConnection);
      auto milli_degc = static_cast<int32_t>(sht3x.cTemp * 1000.0);
      if (!global_var.envDatabaseConnection->insert_temperature(SENSOR_ID_SHT30, nullptr, utc, milli_degc)) {
        M5_LOGD("insert failure");
      }
      auto ppm_rh = static_cast<int32_t>(sht3x.humidity * 10000.0);
      if (!global_var.envDatabaseConnection->insert_relative_humidity(SENSOR_ID_SHT30, nullptr, utc, ppm_rh)) {
        M5_LOGD("insert failure");
      }
    }

    if (bmp.update()) {
      /*
      Serial.println("-----BMP280-----");
      Serial.print(F("Temperature: "));
      Serial.print(bmp.cTemp);
      Serial.println(" degrees C");
      Serial.print(F("Pressure: "));
      Serial.print(bmp.pressure);
      Serial.println(" Pa");
      Serial.print(F("Approx altitude: "));
      Serial.print(bmp.altitude);
      Serial.println(" m");
      Serial.println("-------------\r\n");*/
      /* データーベースに挿入する */
      assert(global_var.envDatabaseConnection);
      auto milli_degc = static_cast<int32_t>(bmp.cTemp * 1000.0);
      if (!global_var.envDatabaseConnection->insert_temperature(SENSOR_ID_BMP280, nullptr, utc, milli_degc)) {
        M5_LOGD("insert failure");
      }
      auto pascal = static_cast<int32_t>(bmp.pressure);
      if (!global_var.envDatabaseConnection->insert_pressure(SENSOR_ID_BMP280, nullptr, utc, pascal)) {
        M5_LOGD("insert failure");
      }
    }
  }
};

/* Arduinoのsetup */
void setup() {
  /* M5UninfiedによるM5Stackの初期化 */
  {
    auto cfg = M5.config();
    M5.begin(cfg);
  }

  /* M5.Logの設定 */
  M5.Log.setEnableColor(m5::log_target_serial, false);
  M5.Log.setLogLevel(m5::log_target_serial, ESP_LOG_VERBOSE);

  /* M5.Displayの設定 */
  M5.Display.setColorDepth(LV_COLOR_DEPTH);
  M5.Display.setBrightness(128);

  /* LVGLの初期化 */
  lv_init();

  /* 時間経過用のtick関数を設定する */
  lv_tick_set_cb(my_tick);

/* デバッグ用コールバックの登録 */
#if LV_USE_LOG != 0
  lv_log_register_print_cb(my_print);
#endif

  /* LVGLディスプレイの設定(縦横方向のピクセル数はM5Unifiedから取得する) */
  lv_display_t *disp = lv_display_create(M5.Display.width(), M5.Display.height());
  lv_display_set_flush_cb(disp, my_disp_flush);
  lv_display_set_buffers(disp, global_var._lvgl_draw_buf, nullptr, sizeof(global_var._lvgl_draw_buf), LV_DISPLAY_RENDER_MODE_PARTIAL);

  /* LVGLタッチパッド入力デバイスの設定 */
  lv_indev_t *indev = lv_indev_create();
  lv_indev_set_type(indev, LV_INDEV_TYPE_POINTER);
  lv_indev_set_read_cb(indev, my_touchpad_read);

  /* LVGL専用RTOSタスクを起動する */
  xTaskCreatePinnedToCore(
    [](void *arg) -> void {
      while (true) {
        M5.update();
        uint32_t delay_time_till_next = lv_timer_handler();
        delay(delay_time_till_next);
      }
    },
    "LVGL", 8192, nullptr, 5, nullptr, ARDUINO_RUNNING_CORE);

  /* ezTime */
  if (global_var.tz_local.setPosix("Asia/Tokyo-9") == false) {
    show_fatal_alert("ローカルタイムの設定に失敗。");
    goto error_halt;
  }

  /* SDカード */
  if (!SD.begin(GPIO_NUM_4, SPI, 25000000)) {
    M5_LOGI("SD card mout failed");
  }

  /* データーベースを初期化する */
  if ((global_var.envDatabaseConnection = EnvDatabaseConnection::open_connection("file:/env_data.db?pow=0&mode=memory")) == nullptr) {
    show_fatal_alert("データーベースの初期化に失敗しました。");
    goto error_halt;
  }

  /* データーベースのテーブル作成 */
  assert(global_var.envDatabaseConnection);
  if (!global_var.envDatabaseConnection->create_all_tables()) {
    show_fatal_alert("データーベースのテーブル作成に失敗しました。");
    goto error_halt;
  }

  /* M5Unit ENV2を初期化する */
  if ((global_var.m5unit_env2 = M5UnitEnv2::begin()) == nullptr) {
    show_fatal_alert("初期化に失敗しました。\nセンサーの接続を確認してください。");
    goto error_halt;
  }

  /* RTCより時刻復帰する */
  restore_datetime_from_RTC();

  /* 画面を用意する */
  global_var.displayed_tabview = create_screen_default();

  /* セットアップ完了 */
  M5_LOGI("Setup done");
  M5.Speaker.tone(2000, 100, 0, true);
  M5.Speaker.tone(1000, 100, 0, false);
  return;

error_halt:
  /* セットアップ失敗のために停止する */
  while (true) { delay(1); }
}

/* Arduinoのloop */
void loop() {
  if (global_var.m5unit_env2) {
    global_var.m5unit_env2->update();
  }

  if (global_var.displayed_tabview) {
    if (M5.BtnA.isPressed()) {
      vibrate(300);
      lv_tabview_set_active(global_var.displayed_tabview, 0, LV_ANIM_ON);
    } else if (M5.BtnB.isPressed()) {
      vibrate(300);
      lv_tabview_set_active(global_var.displayed_tabview, 1, LV_ANIM_ON);
    } else if (M5.BtnC.isPressed()) {
      vibrate(300);
      lv_tabview_set_active(global_var.displayed_tabview, 2, LV_ANIM_ON);
    } else {
      lv_obj_send_event(global_var.displayed_tabview, LV_EVENT_REFRESH, nullptr);
    }
  }

  /* 次回呼び出しまでの時間を決める */
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
    M5_LOGE("clock_gettime() is not allowed");
    return;
  }
  uint32_t delay_to_ms = 1000UL - (ts.tv_nsec / 1000UL / 1000UL);
  delay(delay_to_ms);
}

/*
 * 関数定義
 */

/* use Arduinos millis() as tick source */
uint32_t my_tick() {
  return millis();
}

/* デバッグ用 */
#if LV_USE_LOG != 0
void my_print(lv_log_level_t level, const char *buf) {
  LV_UNUSED(level);
  M5_LOGD("%s", buf);
}
#endif

/* LVGLによってレンダリングされた描画をディスプレイに送る */
void my_disp_flush(lv_display_t *disp, const lv_area_t *area, uint8_t *px_map) {
  uint32_t width = area->x2 - area->x1 + 1;
  uint32_t height = area->y2 - area->y1 + 1;
  lv_draw_sw_rgb565_swap(px_map, width * height);
  M5.Display.pushImageDMA<uint16_t>(area->x1, area->y1, width, height, reinterpret_cast<uint16_t *>(px_map));
  lv_display_flush_ready(disp);
}

/* タッチパッド入力 */
void my_touchpad_read(lv_indev_t *indev, lv_indev_data_t *data) {
  LV_UNUSED(indev);
  lgfx::touch_point_t tp;
  if (M5.Display.getTouch(&tp)) {
    data->state = LV_INDEV_STATE_PRESSED;
    data->point.x = tp.x;
    data->point.y = tp.y;
  } else {
    data->state = LV_INDEV_STATE_RELEASED;
  }
}

/* 致命的なエラー用のアラートを表示する */
void show_fatal_alert(const String &message) {
  /* メッセージボックス */
  lv_obj_t *mbox1 = lv_msgbox_create(nullptr);

  lv_msgbox_add_title(mbox1, "もうどうしようもない");
  lv_obj_t *title = lv_msgbox_get_title(mbox1);
  if (title) {
    lv_obj_set_style_text_font(title, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  lv_msgbox_add_text(mbox1, message.c_str());
  lv_obj_t *content = lv_msgbox_get_content(mbox1);
  if (content) {
    lv_obj_set_style_text_font(content, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  /* ボタン */
  lv_obj_t *btn;
  btn = lv_msgbox_add_footer_button(mbox1, "リセット");
  if (btn) {
    lv_obj_set_style_text_font(btn, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  /* CLICKEDイベントハンドラ */
  lv_obj_add_event_cb(
    btn, [](lv_event_t *e) {
      auto it = static_cast<lv_obj_t *>(lv_event_get_user_data(e));
      if (it) {
        lv_msgbox_close(it);
      }
      /* 再起動 */
      system_restart();
    },
    LV_EVENT_CLICKED, mbox1);
}

/* RTCより時刻復帰する */
void restore_datetime_from_RTC() {
  if (M5.Rtc.isEnabled()) {
    /* RTCより現在時刻を得る */
    m5::rtc_datetime_t dt;
    M5.Rtc.getDateTime(&dt);
    /* 現在時刻をセットする */
    struct tm timeinfo = dt.get_tm();
    global_var.tz_local.setTime(mktime(&timeinfo));
    /**/
    String utc_now = dateTime();
    M5_LOGD("%s", utc_now.c_str());
    String local_now = global_var.tz_local.dateTime();
    M5_LOGD("%s", local_now.c_str());
  } else {
    show_fatal_alert("RTC機能が動作していません。");
  }
}

/* バイブレーション */
void vibrate(uint16_t millisec) {
  M5.Power.Axp192.setLDO3(3300);
  lv_timer_t *timer =
    lv_timer_create([](lv_timer_t *) -> void {
      M5.Power.Axp192.setLDO3(0);
    },
                    millisec, nullptr);
  lv_timer_set_repeat_count(timer, 1);
  lv_timer_set_auto_delete(timer, true);
}

/* 再起動 */
void system_restart() {
  M5.Speaker.tone(4000);
  delay(600);
  M5.Speaker.end();
  esp_restart();
}

namespace home_tab {
/* */
void draw_event_cb(lv_event_t *e) {
  lv_draw_task_t *draw_task = lv_event_get_draw_task(e);
  auto base_dsc = static_cast<lv_draw_dsc_base_t *>(lv_draw_task_get_draw_dsc(draw_task));

  /*If the cells are drawn...*/
  if (base_dsc->part == LV_PART_ITEMS) {
    uint32_t row = base_dsc->id1;
    uint32_t col = base_dsc->id2;

    if (col == 0) {
      lv_draw_label_dsc_t *label_draw_dsc = lv_draw_task_get_label_dsc(draw_task);
      if (label_draw_dsc) {
        label_draw_dsc->font = FONT_JA_JP;
        label_draw_dsc->align = LV_TEXT_ALIGN_RIGHT;
      }
    } else if (col == 1) {
      lv_draw_label_dsc_t *label_draw_dsc = lv_draw_task_get_label_dsc(draw_task);
      if (label_draw_dsc) {
        label_draw_dsc->font = &lv_font_montserrat_16;
        label_draw_dsc->align = LV_TEXT_ALIGN_RIGHT;
      }
      lv_draw_fill_dsc_t *fill_draw_dsc = lv_draw_task_get_fill_dsc(draw_task);
      if (fill_draw_dsc) {
        fill_draw_dsc->color = lv_color_mix(lv_palette_main(LV_PALETTE_INDIGO), fill_draw_dsc->color, LV_OPA_10);
        fill_draw_dsc->opa = LV_OPA_COVER;
      }
    } else if (col == 2) {
      lv_draw_label_dsc_t *label_draw_dsc = lv_draw_task_get_label_dsc(draw_task);
      if (label_draw_dsc) {
        label_draw_dsc->font = FONT_JA_JP;
        label_draw_dsc->align = LV_TEXT_ALIGN_LEFT;
      }
    }
  }
}

/*  */
void table_refresh_event_handler(lv_event_t *e) {
  auto table = static_cast<lv_obj_t *>(lv_event_get_user_data(e));

  if (global_var.m5unit_env2 && table) {
    assert(global_var.m5unit_env2);
    float values[] = {
      global_var.m5unit_env2->sht3x.cTemp,
      global_var.m5unit_env2->sht3x.humidity,
      global_var.m5unit_env2->bmp.cTemp,
      global_var.m5unit_env2->bmp.pressure / 100.0f,
      global_var.m5unit_env2->bmp.altitude,
    };
    for (uint32_t idx = 0; idx < sizeof(values) / sizeof(values[0]); ++idx) {
      lv_table_set_cell_value_fmt(table, idx, 1, "%4.02f", values[idx]);
    }
  }
}

/*  */
void datetime_label_refresh_event_handler(lv_event_t *e) {
  auto label = static_cast<lv_obj_t *>(lv_event_get_user_data(e));

  if (label) {
    auto dt = global_var.tz_local.dateTime(RFC3339);
    lv_label_set_text(label, dt.c_str());
  }
}

/* ホームタブ */
lv_obj_t *create_home_tab(lv_obj_t &tab_view) {
  lv_obj_t *tab = lv_tabview_add_tab(&tab_view, LV_SYMBOL_HOME);
  lv_obj_set_style_border_width(tab, 0, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_set_style_pad_all(tab, 0, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_set_size(tab, lv_pct(100), lv_pct(100));
  lv_obj_remove_flag(tab, LV_OBJ_FLAG_SCROLLABLE);

  /* ラベル */
  lv_obj_t *label = lv_label_create(tab);
  lv_obj_set_style_pad_all(label, 5, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_set_style_text_font(label, &lv_font_montserrat_16, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_set_size(label, lv_pct(100), LV_SIZE_CONTENT);
  lv_obj_set_style_text_align(label, LV_TEXT_ALIGN_RIGHT, 0);
  lv_obj_align(label, LV_ALIGN_BOTTOM_LEFT, 0, 0);

  lv_label_set_text(label, "time");

  lv_obj_update_layout(tab);

  /* テーブル */
  lv_obj_t *table = lv_table_create(tab);
  lv_obj_set_size(table, lv_pct(100), lv_obj_get_content_height(tab) - lv_obj_get_height(label));
  lv_obj_align_to(table, label, LV_ALIGN_OUT_TOP_LEFT, 0, 0);

  auto width = lv_obj_get_content_width(table);
  lv_table_set_column_width(table, 0, 5 * width / 12);
  lv_table_set_column_width(table, 1, 4 * width / 12);
  lv_table_set_column_width(table, 2, 3 * width / 12);

  /* */
  lv_table_set_cell_value(table, 0, 0, "温度 SHT30");
  lv_table_set_cell_value(table, 1, 0, "しつ度 SHT30");
  lv_table_set_cell_value(table, 2, 0, "温度 BMP280");
  lv_table_set_cell_value(table, 3, 0, "気あつ BMP280");
  lv_table_set_cell_value(table, 4, 0, "高度 BMP280");

  /* */
  lv_table_set_cell_value(table, 0, 1, "-");
  lv_table_set_cell_value(table, 1, 1, "-");
  lv_table_set_cell_value(table, 2, 1, "-");
  lv_table_set_cell_value(table, 3, 1, "-");
  lv_table_set_cell_value(table, 4, 1, "-");

  /* */
  lv_table_set_cell_value(table, 0, 2, "度");
  lv_table_set_cell_value(table, 1, 2, "%");
  lv_table_set_cell_value(table, 2, 2, "度");
  lv_table_set_cell_value(table, 3, 2, "hPa");
  lv_table_set_cell_value(table, 4, 2, "m");

  /*Add an event callback to to apply some custom drawing*/
  lv_obj_add_event_cb(table, draw_event_cb, LV_EVENT_DRAW_TASK_ADDED, NULL);
  lv_obj_add_flag(table, LV_OBJ_FLAG_SEND_DRAW_TASK_EVENTS);

  lv_obj_add_event_cb(tab, table_refresh_event_handler, LV_EVENT_REFRESH, table);
  lv_obj_add_event_cb(tab, datetime_label_refresh_event_handler, LV_EVENT_REFRESH, label);

  return tab;
}
} /* namespace home_tab */

namespace setting_date_and_time_tab {
/* ドロップダウンとラベルのペア */
class DropdownWithLabel {
  /* ラベル用スタイル */
  lv_style_t label_style;
public:
  lv_obj_t *pane;
  lv_obj_t *dropdown;
  lv_obj_t *label;
  /* コンストラクタ */
  DropdownWithLabel(lv_obj_t &parent, const String &label_text) {
    lv_style_init(&label_style);
    /* 日本語表示フォントを設定する */
    lv_style_set_text_font(&label_style, FONT_JA_JP);
    /* 台 */
    pane = lv_obj_create(&parent);
    lv_obj_set_size(pane, LV_SIZE_CONTENT, LV_SIZE_CONTENT);
    lv_obj_set_style_border_width(pane, 0, LV_PART_MAIN | LV_STATE_DEFAULT);
    lv_obj_set_style_pad_all(pane, 5, LV_PART_MAIN | LV_STATE_DEFAULT);
    /* ドロップダウン */
    dropdown = lv_dropdown_create(pane);
    /* ラベル */
    lv_obj_t *label = lv_label_create(pane);
    lv_label_set_text(label, label_text.c_str());
    lv_obj_add_style(label, &label_style, LV_PART_MAIN | LV_STATE_DEFAULT);
    lv_obj_align_to(label, dropdown, LV_ALIGN_OUT_RIGHT_MID, 10, 0);
  }
};

/* 年月日時分秒ドロップダウン */
class YMDHMS {
public:
  DropdownWithLabel year;
  DropdownWithLabel month;
  DropdownWithLabel day;
  DropdownWithLabel hour;
  DropdownWithLabel minute;
  DropdownWithLabel sec;
  /* コンストラクタ */
  YMDHMS(lv_obj_t &parent)
    : year{ parent, "年" },
      month{ parent, "月" },
      day{ parent, "日" },
      hour{ parent, "時" },
      minute{ parent, "分" },
      sec{ parent, "びょう" } {
    /* 設定 */
    adjust_dropdown_now(global_var.tz_local);
  }
  /* 選択を現在時刻にする */
  void align_to(lv_obj_t &above) {
    /* 年 */
    lv_obj_align_to(year.pane, &above, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 0);
    /* 月 */
    lv_obj_align_to(month.pane, year.pane, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 0);
    /* 日 */
    lv_obj_align_to(day.pane, month.pane, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 0);
    /* 時 */
    lv_obj_align_to(hour.pane, day.pane, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 0);
    /* 分 */
    lv_obj_align_to(minute.pane, hour.pane, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 0);
    /* 秒 */
    lv_obj_align_to(sec.pane, minute.pane, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 0);
  }
  /* 選択を現在時刻にする */
  void adjust_dropdown_now(Timezone &timezone) {
    char buf[100];
    String options{};
    auto compiled_year = timezone.year(compileTime());
    /* 年 */
    options.clear();
    for (int i = compiled_year; i < compiled_year + 10; ++i) {
      lv_snprintf(buf, sizeof(buf), "%4d\n", i);
      options += buf;
    }
    options.trim();
    lv_dropdown_set_options(year.dropdown, options.c_str());
    auto Y = 0;
    lv_dropdown_set_selected(year.dropdown, Y);
    /* 月 */
    options.clear();
    for (int i = 1; i <= 12; ++i) {
      lv_snprintf(buf, sizeof(buf), "%2d\n", i);
      options += buf;
    }
    options.trim();
    lv_dropdown_set_options(month.dropdown, options.c_str());
    auto M = timezone.month();
    lv_dropdown_set_selected(month.dropdown, M - 1);
    /* 日 */
    options.clear();
    for (int i = 1; i <= 31; ++i) {
      lv_snprintf(buf, sizeof(buf), "%2d\n", i);
      options += buf;
    }
    options.trim();
    lv_dropdown_set_options(day.dropdown, options.c_str());
    auto D = timezone.day();
    lv_dropdown_set_selected(day.dropdown, D - 1);
    /* 時 */
    options.clear();
    for (int i = 0; i <= 23; ++i) {
      lv_snprintf(buf, sizeof(buf), "%02d\n", i);
      options += buf;
    }
    options.trim();
    lv_dropdown_set_options(hour.dropdown, options.c_str());
    auto h = timezone.hour();
    lv_dropdown_set_selected(hour.dropdown, h);
    /* 分 */
    options.clear();
    for (int i = 0; i <= 59; ++i) {
      lv_snprintf(buf, sizeof(buf), "%02d\n", i);
      options += buf;
    }
    options.trim();
    lv_dropdown_set_options(minute.dropdown, options.c_str());
    auto m = timezone.minute();
    lv_dropdown_set_selected(minute.dropdown, m);
    /* 秒 */
    options.clear();
    for (int i = 0; i <= 59; ++i) {
      lv_snprintf(buf, sizeof(buf), "%02d\n", i);
      options += buf;
    }
    options.trim();
    lv_dropdown_set_options(sec.dropdown, options.c_str());
    auto s = timezone.second();
    lv_dropdown_set_selected(sec.dropdown, s);
  }
  /* 選択時間をRTCに書込む */
  void set_date_time_to_rtc() {
    char Y[20], M[20], D[20];
    char h[20], m[20], s[20];
    lv_dropdown_get_selected_str(year.dropdown, Y, sizeof(Y));
    lv_dropdown_get_selected_str(month.dropdown, M, sizeof(M));
    lv_dropdown_get_selected_str(day.dropdown, D, sizeof(D));
    lv_dropdown_get_selected_str(hour.dropdown, h, sizeof(h));
    lv_dropdown_get_selected_str(minute.dropdown, m, sizeof(m));
    lv_dropdown_get_selected_str(sec.dropdown, s, sizeof(s));
    M5_LOGD("%s-%s-%s %s:%s:%s", Y, M, D, h, m, s);
    struct tm tminfo {};
    tminfo.tm_sec = atoi(s);
    tminfo.tm_min = atoi(m);
    tminfo.tm_hour = atoi(h);
    tminfo.tm_mday = atoi(D);
    tminfo.tm_mon = atoi(M) - 1;
    tminfo.tm_year = atoi(Y) - 1900;
    char buf[255];
    strftime(buf, sizeof(buf), "%Y-%m-%dT%T %Z", &tminfo);
    M5_LOGD("%s", buf);
    /* RTC設定 */
    if (M5.Rtc.isEnabled()) {
      M5.Rtc.setDateTime(&tminfo);
      /* 再起動 */
      system_restart();
    } else {
      show_fatal_alert("RTC機能が動作していません。");
    }
  }
};

/* 時刻設定タブ */
lv_obj_t *create_setting_date_and_time_tab(lv_obj_t &tab_view) {
  lv_obj_t *tab = lv_tabview_add_tab(&tab_view, LV_SYMBOL_SETTINGS);

  /* ラベル */
  lv_obj_t *label = lv_label_create(tab);
  lv_label_set_text(label, "現在時刻設定");
  lv_obj_set_style_text_font(label, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_align(label, LV_ALIGN_TOP_LEFT, 0, 0);

  /* これはstatic領域に配置 */
  static YMDHMS *ymdhms{ nullptr };
  if (ymdhms == nullptr) {
    assert(tab);
    ymdhms = new YMDHMS(*tab);
  }
  assert(ymdhms);
  assert(label);
  ymdhms->align_to(*label);
  /* VALUE_CHANGEDコールバック */
  lv_obj_add_event_cb(
    tab, [](lv_event_t *e) -> void {
      lv_event_code_t code = lv_event_get_code(e);
      if (code == LV_EVENT_VALUE_CHANGED) {
        auto it = static_cast<YMDHMS *>(lv_event_get_user_data(e));
        if (it) {
          it->adjust_dropdown_now(global_var.tz_local);
        }
      }
    },
    LV_EVENT_VALUE_CHANGED, ymdhms);

  /* 設定ボタン */
  lv_obj_t *btn = lv_button_create(tab);
  /* CLICKEDコールバック */
  lv_obj_add_event_cb(
    btn,
    [](lv_event_t *e) -> void {
      lv_event_code_t code = lv_event_get_code(e);
      if (code == LV_EVENT_CLICKED) {
        auto it = static_cast<YMDHMS *>(lv_event_get_user_data(e));
        if (it) {
          it->set_date_time_to_rtc();
        }
      }
    },
    LV_EVENT_CLICKED, ymdhms);
  lv_obj_align_to(btn, ymdhms->sec.pane, LV_ALIGN_OUT_BOTTOM_RIGHT, 0, 10);

  label = lv_label_create(btn);
  lv_label_set_text(label, "RTCに書込む");
  lv_obj_set_style_text_font(label, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_center(label);

  return tab;
}
} /* namespace setting_date_and_time_tab */

namespace miscellaneous_tab {

void clicked_event_handler(lv_event_t *e) {
  auto it = static_cast<lv_obj_t *>(lv_event_get_user_data(e));
  if (it) {
    lv_msgbox_close(it);
  }
  /* メッセージボックス */
  lv_obj_t *mbox1 = lv_msgbox_create(nullptr);
  lv_msgbox_add_close_button(mbox1);

  lv_msgbox_add_title(mbox1, "データーベースをSDに保存する");
  lv_obj_t *title = lv_msgbox_get_title(mbox1);
  if (title) {
    lv_obj_set_style_text_font(title, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  lv_obj_t *content = lv_msgbox_get_content(mbox1);
  if (content) {
    lv_obj_set_style_text_font(content, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  /* ラベル */
  lv_obj_t *label = lv_label_create(content);
  if (label) {
    lv_obj_set_style_text_font(label, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }
  lv_label_set_text(label, "実行中");
  lv_obj_center(label);

  assert(global_var.envDatabaseConnection);
  if (global_var.envDatabaseConnection->save_to_file("file:/sd/env_data.db")) {
    lv_label_set_text(label, "完了");
  } else {
    lv_label_set_text(label, "失敗");
  }
}

/* ファイルに保存メッセージボックスを表示する */
void show_save_to_file_message_box() {
  /* メッセージボックス */
  lv_obj_t *mbox1 = lv_msgbox_create(nullptr);

  lv_msgbox_add_title(mbox1, "データーベースをSDに保存する");
  lv_obj_t *title = lv_msgbox_get_title(mbox1);
  if (title) {
    lv_obj_set_style_text_font(title, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  lv_obj_t *content = lv_msgbox_get_content(mbox1);
  if (content) {
    lv_obj_set_style_text_font(content, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  /* ラベル */
  lv_obj_t *label = lv_label_create(content);
  lv_label_set_text(label, "SDカードをいれてOKをおす");
  lv_obj_set_style_text_font(label, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_align(label, LV_ALIGN_TOP_LEFT, 0, 0);

  /* ボタン */
  lv_obj_t *btn;
  btn = lv_msgbox_add_footer_button(mbox1, "OK");
  if (btn) {
    lv_obj_set_style_text_font(btn, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  }

  /* CLICKEDイベントハンドラ */
  lv_obj_add_event_cb(btn, clicked_event_handler, LV_EVENT_CLICKED, mbox1);
}

/*  */
lv_obj_t *create_misc_tab(lv_obj_t &tab_view) {
  lv_obj_t *tab = lv_tabview_add_tab(&tab_view, LV_SYMBOL_POWER);

  /* 保存ボタン */
  lv_obj_t *save_to_file_btn = lv_button_create(tab);
  /* CLICKEDコールバック */
  lv_obj_add_event_cb(
    save_to_file_btn,
    [](lv_event_t *e) -> void {
      show_save_to_file_message_box();
    },
    LV_EVENT_CLICKED, nullptr);
  lv_obj_align(save_to_file_btn, LV_ALIGN_TOP_MID, 0, 0);

  lv_obj_t *label = lv_label_create(save_to_file_btn);
  lv_label_set_text(label, "SDに保存");
  lv_obj_set_style_text_font(label, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_center(label);

  /* 電源オフボタン */
  lv_obj_t *power_off_btn = lv_button_create(tab);
  /* CLICKEDコールバック */
  lv_obj_add_event_cb(
    power_off_btn,
    [](lv_event_t *e) -> void {
      lv_event_code_t code = lv_event_get_code(e);
      if (code == LV_EVENT_CLICKED) {
        /* ビープ */
        M5.Speaker.tone(4000);
        delay(600);
        M5.Speaker.end();
        M5.Power.powerOff();
      }
    },
    LV_EVENT_CLICKED, nullptr);
  lv_obj_align_to(power_off_btn, save_to_file_btn, LV_ALIGN_OUT_BOTTOM_LEFT, 0, 50);

  label = lv_label_create(power_off_btn);
  lv_label_set_text(label, "電源オフ");
  lv_obj_set_style_text_font(label, FONT_JA_JP, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_center(label);

  return tab;
}
} /* namespace miscellaneous_tab */

/* 初期画面 */
lv_obj_t *create_screen_default() {
  const lv_font_t *FONT_TAB_BAR{ &lv_font_montserrat_28 };

  lv_obj_t *tabview = lv_tabview_create(nullptr);
  lv_tabview_set_tab_bar_size(tabview, FONT_TAB_BAR->line_height + 10);

  /* タブボタンのスタイル */
  lv_obj_t *tab_buttons = lv_tabview_get_tab_bar(tabview);
  lv_obj_set_style_text_color(tab_buttons, lv_palette_main(LV_PALETTE_YELLOW), LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_set_style_text_font(tab_buttons, FONT_TAB_BAR, LV_PART_MAIN | LV_STATE_DEFAULT);
  lv_obj_set_style_bg_color(tab_buttons, lv_palette_darken(LV_PALETTE_GREY, 3), LV_PART_MAIN | LV_STATE_DEFAULT);

  /* タブを追加する */
  lv_obj_t *home_tab = home_tab::create_home_tab(*tabview);
  lv_obj_t *settings_tab = setting_date_and_time_tab::create_setting_date_and_time_tab(*tabview);
  lv_obj_t *misc_tab = miscellaneous_tab::create_misc_tab(*tabview);

  /* イベントを子要素に配分する */
  lv_obj_add_event_cb(
    tabview, [](lv_event_t *e) -> void {
      auto tv = static_cast<lv_obj_t *>(lv_event_get_target(e));
      assert(tv);
      lv_obj_t *content = lv_tabview_get_content(tv);
      if (!content) {
        return;
      }
      lv_event_code_t code = lv_event_get_code(e);
      if (code == LV_EVENT_VALUE_CHANGED || code == LV_EVENT_REFRESH) {
        assert(content);
        for (auto idx = 0; idx < lv_obj_get_child_count(content); ++idx) {
          lv_obj_t *child = lv_obj_get_child(content, idx);
          assert(child);
          lv_obj_send_event(child, code, nullptr);
        }
      }
    },
    LV_EVENT_ALL, nullptr);
  lv_tabview_set_active(tabview, 0, LV_ANIM_OFF);

  lv_screen_load(tabview);

  return tabview;
}
