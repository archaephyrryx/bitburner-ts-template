import { AutocompleteData, NS } from "@ns";

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
            if (ns.singularity.getCurrentWork() === null || ns.singularity.getCurrentWork().type !== "CREATE_PROGRAM") {
                return false;
            }
            await ns.sleep(1000);
        }
        return true;
    } else {
        return false;
    }
}


export async function getScript(ns: NS, which: `${ScriptFile}`, forceTor = false) {
    if (ns.fileExists(which, "home")) {
        return;
    }
    if (!forceTor) {
        const done = await createScript(ns, which);
        if (done) return true;
    }

    while (!ns.singularity.purchaseTor()) {
        await ns.sleep(1000);
    }

    while (!ns.singularity.purchaseProgram(which)) {
        await ns.sleep(1000);
    }

    return;
}


export async function main(ns: NS) {
    const flags = ns.flags([["tor", false]])
    const which = ns.args[0] as string ?? "SQLInject.exe";

    function lastDitch() {
        if (ns.singularity.purchaseTor()) {
            ns.singularity.purchaseProgram(which);
        }
    }

    if (ns.fileExists(which, "home")) {
        return;
    } else {
        ns.atExit(lastDitch);
        await getScript(ns, which as `${ScriptFile}`, flags.tor as boolean);
    }
}

export function autocomplete(data: AutocompleteData, ...args: string[]) {
    data.flags([["tor", false]]);
    return [ScriptFile.BruteSSH, ScriptFile.FTPCrack, ScriptFile.HTTPWorm, ScriptFile.SQLInject, ScriptFile.relaySMTP];
}
