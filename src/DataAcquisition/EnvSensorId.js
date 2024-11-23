export const printEnvSensorIdImpl = (input) => {
    let dv = new DataView(new ArrayBuffer(10));
    dv.setBigUint64(0, BigInt(input), false);
    let result = new Uint8Array(dv.buffer).reduce((acc, item) => { if (item == 0x00) { return acc } else { return acc.concat(String.fromCharCode(item)) } }, "")
    return result
}