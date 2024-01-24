import { NS } from "@ns";

export async function main(ns: NS) {
    const oldFloor = Math.floor;
    const oldRandom = Math.random;
    for (; ;) {
        let exploited = false;
        if (ns.getPlayer().location === "Iker Molina Casino") {
            if (!exploited) {
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                Math.floor = _n => { return 1 };
                Math.random = () => { return 0 };
                exploited = true;
            } else {
                await ns.sleep(1000);
            }
        } else {
            if (!exploited) {
                await ns.sleep(1000);
                continue;
            }
            break;
        }
    }
    Math.floor = oldFloor;
    Math.random = oldRandom;
}
