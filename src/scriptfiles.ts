import { NS } from "@ns";

export const Costs = {
    torRouter: 200_000,
} as const;

export enum ScriptFile {
    BruteSSH = "BruteSSH.exe",
    FTPCrack = "FTPCrack.exe",
    relaySMTP = "relaySMTP.exe",
    HTTPWorm = "HTTPWorm.exe",
    SQLInject = "SQLInject.exe",
}

// export const ScriptCosts: { [scriptname in `${ScriptFile}`]: number } = {
//     ["HTTPWorm.exe"]: 30_000_00,
//     ["SQLInject.exe"]: 250_000_000,
// }

export async function createScript(ns: NS, script: `${ScriptFile}`): Promise<boolean> {
    const startedWorking = ns.singularity.createProgram(script);
    if (startedWorking) {
        while (!ns.fileExists(script, "home")) {
            await ns.sleep(1000);
        }
        return true;
    } else {
        return false;
    }
}


export async function getScript(ns: NS, which: `${ScriptFile}`) {
    if (ns.fileExists(which, "home")) {
        return;
    }
    const done = await createScript(ns, which);

    if (done) return true;

    while (!ns.singularity.purchaseTor()) {
        await ns.sleep(1000);
    }

    while (!ns.singularity.purchaseProgram(which)) {
        await ns.sleep(1000);
    }

    return;
}
