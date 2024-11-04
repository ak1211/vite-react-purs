import {
    sqlite3Worker1Promiser
} from '@sqlite.org/sqlite-wasm';

/* SQLite3 WASM Worker1 Promiser API */
export const createWorker1PromiserImpl = () => { return sqlite3Worker1Promiser.v2({}); };

export const configGetImpl = (promiser) => { return promiser('config-get', {}); };

export const openImpl = (promiser, opfsFilePath) => {
    return promiser("open", { filename: "file:" + opfsFilePath + "?vfs=opfs" });
};

export const closeImpl = (promiser, dbId) => {
    return promiser("close", { dbId: dbId });
};

export const execImpl = (promiser, dbId, sql) => {
    return promiser("exec", {
        dbId: dbId,
        sql: sql,
        rowMode: "object",
        returnValue: "resultRows",
    });
};

export const overwriteOpfsFileWithSpecifiedArrayBufferImpl = (
    opfsFilePath,
    arrayBuffer
) => {
    const writable = navigator.storage.getDirectory()
        .then(opfsRoot => { return opfsRoot.getFileHandle(opfsFilePath, { create: true }) })
        .then(fileHandle => { return fileHandle.createWritable() });
    return Promise.all([writable, arrayBuffer])
        .then(([w, ab]) => { w.write(ab).then(_ => { return w.close() }) });
};
