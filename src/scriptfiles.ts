import { AutocompleteData, NS } from "@ns";
import { CompanyWorkTask, CreateProgramWorkTask, CrimeTask, FactionWorkTask, GraftingTask, StudyTask, getWork } from "./global";

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
    if (ns.bladeburner.inBladeburner() && ns.bladeburner.getCurrentAction().type !== 'Idle')
        return false;
    const prevWork = getWork(ns);
    if (prevWork !== null) {
        switch (prevWork.type) {
            case 'CLASS':
                ns.singularity.stopAction();
                break;
            case 'COMPANY':
                ns.singularity.stopAction();
                break;
            case 'CREATE_PROGRAM':
                if (prevWork.programName == script) {
                    while (getWork(ns) === prevWork) {
                        await ns.sleep(1000);
                    }
                    break;
                }
                ns.singularity.stopAction()
                break;
            case 'CRIME':
                ns.singularity.stopAction();
                break;
            case 'FACTION':
                ns.singularity.stopAction();
                break;
            case 'GRAFTING':
                return false;
        }
    }

    if (ns.fileExists(script, "home")) {
        return true;
    }
    const startedWorking = ns.singularity.createProgram(script);
    if (startedWorking) {
        while (!haveScript(ns, script)) {
            const work = getWork(ns);
            if (work === null || work.type !== "CREATE_PROGRAM") {
                return false;
            }
            await ns.sleep(1000);
        }
        return true;
    } else {
        return false;
    }
}

export function haveScript(ns: NS, script: `${ScriptFile}` | ScriptFile): boolean {
    return ns.fileExists(script, "home");
}


export async function getScript(ns: NS, which: `${ScriptFile}`, forceTor = false) {
    if (haveScript(ns, which)) {
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
