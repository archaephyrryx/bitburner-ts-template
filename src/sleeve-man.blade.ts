import { AutocompleteData, NS } from "@ns";
import { SleeveTask } from "./global";
import { M, bold, formatTime, reset } from "./helper";

const SYNCHRO_THRESHOLD = 50;
const SHOCK_THRESHOLD = 90;

const cyclesForInfiltrate = M * 1000 / 200;


export async function main(ns: NS) {
    const nSleeves = ns.sleeve.getNumSleeves();
    await initiateBlade(ns, nSleeves);
    for (; ;) {
        await runBlade(ns, nSleeves);
    }
}

async function initiateBlade(ns: NS, count: number) {
    for (let i = 0; i < count; i++) {
        const task = ns.sleeve.getTask(i);
        if (task === null) {
            continue;
        } else {
            switch (task.type) {
                case 'INFILTRATE':
                case 'BLADEBURNER':
                case 'FACTION':
                case 'COMPANY':
                case 'CLASS':
                case 'CRIME':
                case 'SUPPORT':
                    ns.sleeve.setToIdle(i);
                    break;
                case 'SYNCHRO':
                    if (ns.sleeve.getSleeve(i).sync >= SYNCHRO_THRESHOLD) {
                        ns.sleeve.setToIdle(i);
                    }
                    break;
                case 'RECOVERY':
                    if (ns.sleeve.getSleeve(i).shock <= SHOCK_THRESHOLD) {
                        ns.sleeve.setToIdle(i);
                    }
                    break;
            }
        }
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    return [];
}

async function runBlade(ns: NS, count: number) {
    ns.disableLog('sleep');
    ns.tail();

    const workers: Set<number> = new Set();
    const bonusCycles: number[] = [];

    for (let i = 0; i < count; i++) {
        bonusCycles.push(ns.sleeve.getSleeve(i).storedCycles);
        const task = ns.sleeve.getTask(i);
        if (task === null) {
            workers.add(i);
        } else {
            switch (task.type) {
                case 'INFILTRATE':
                    workers.add(i);
                    ns.sleeve.setToIdle(i);
                    break;
                default:
                    break;
            }
        }
    }

    let infiltratingIndex = -1;
    for (; ;) {
        ns.clearLog();
        const dftWait = 1000;

        let msToWait;

        const bestCycles = Math.max(...bonusCycles.filter((_, ix) => workers.has(ix)));
        let bestIndex;

        if (infiltratingIndex == -1) {
            bestIndex = bonusCycles.findIndex((value, ix) => workers.has(ix) && value == bestCycles);
        } else {
            bestIndex = infiltratingIndex;
        }

        sleeveloop: for (let i = 0; i < count; i++) {
            let task = ns.sleeve.getTask(i);
            bonusCycles[i] = ns.sleeve.getSleeve(i).storedCycles;
            if (!workers.has(i)) {
                ns.print(`INFO: Skipping non-worker sleeve ${i} (current action: ${getVerb(task)})`)
            } else {
                if (bestIndex == i) {
                    while (task == null || task.type != "INFILTRATE") {
                        if (ns.sleeve.getSleeve(i).storedCycles < cyclesForInfiltrate) {
                            ns.print(`INFO: Not enough stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to idle...`);
                            infiltratingIndex = -1;
                            ns.sleeve.setToIdle(i);
                            bestIndex = bonusCycles.findIndex((value, ix) => workers.has(ix) && value == bestCycles);
                            continue sleeveloop;
                        }
                        if (!ns.sleeve.setToBladeburnerAction(i, "Infiltrate Synthoids")) {
                            ns.tprint(`ERROR: Unable to set sleeve ${i} to 'Infiltrate Synthoids'`);
                        }
                        infiltratingIndex = i;
                        task = ns.sleeve.getTask(i);
                    }
                    const cyclesRemaining = task.cyclesNeeded - task.cyclesWorked;
                    const myBonus = ns.sleeve.getSleeve(i).storedCycles;
                    const walltimeCycles = (cyclesRemaining <= myBonus) ? cyclesRemaining / 3 : (myBonus / 3) + (cyclesRemaining - myBonus);
                    msToWait = (200 * walltimeCycles / 5 + 100);
                    ns.print(`SUCCESS: Sleeve ${i} currently infiltrating, ${formatTime(msToWait / 1000)} left...`);
                } else {
                    ns.print(`INFO: Not enough stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to idle...`);
                    ns.sleeve.setToIdle(i);
                }
            }
        }

        if (msToWait == undefined) {
            await ns.sleep(dftWait);
        } else {
            ns.print(`${bold}Infiltration will be done in <=${formatTime(msToWait / 1000)}.${reset}`);
            await ns.sleep(msToWait);
            ns.sleeve.setToIdle(infiltratingIndex);
        }
    }
}

function getVerb(task: SleeveTask | null): string {
    if (task == null) {
        return "Idle";
    } else {
        switch (task.type) {
            case 'BLADEBURNER':
                return `Blade:${task.actionType}:${task.actionName}`;
            case 'CLASS':
                return `Study:${task.classType}`;
            case 'COMPANY':
                return `Company:${task.companyName}`;
            case 'CRIME':
                return `CRIME:${task.crimeType}`;
            case 'FACTION':
                return `Faction:${task.factionName}:${task.factionWorkType}`;
            default:
                return task.type as string;
        }
    }
}

function millisUntilFinished(cyclesRemaining: number, bonusCycles: number) {
    if (bonusCycles == 0) {
        return 200 * cyclesRemaining;
    } else if (bonusCycles > cyclesRemaining) {
        return 200 * Math.ceil(cyclesRemaining / 3);
    }
}
