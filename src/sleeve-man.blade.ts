import { AutocompleteData, BladeburnerContractName, NS, SleeveTask } from "@ns";
import { M, bold, formatTime, reset } from "./helper";
import { CHAOS_LIMIT } from "./blade";

const SYNCHRO_THRESHOLD = 50;

const cyclesForInfiltrate = M * 1000 / 200;

/**
 * Amount of stored bonus cycles we don't need to worry about depleting
 * if we meet or exceed.
*/
const BONUS_OVERKILL = 1_000_000;

export async function main(ns: NS) {
    const flags = ns.flags([["--doContracts", false]]);
    const nSleeves = ns.sleeve.getNumSleeves();
    await initiateBlade(ns, nSleeves);
    for (; ;) {
        await runBlade(ns, nSleeves, flags.doContracts as boolean);
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
                    if (ns.sleeve.getSleeve(i).shock == 0) {
                        ns.sleeve.setToIdle(i);
                    }
                    break;
            }
        }
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["--doContracts", false]]);
    return [];
}

async function runBlade(ns: NS, count: number, doContracts: boolean) {
    ns.disableLog('sleep');
    ns.ui.openTail();

    const workers: Set<number> = new Set();
    const bonusCycles: number[] = [];

    let infiltratingIndex = -1;
    let contractIndex = -1;

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
                case 'RECOVERY':
                    workers.add(i);
                    break;
                case 'BLADEBURNER':
                    workers.add(i);
                    ns.sleeve.setToIdle(i);
                    break;
                default:
                    break;
            }
        }
    }

    for (; ;) {
        ns.clearLog();
        const dftWait = 1000;

        let msToWait;

        const bestCycles = Math.max(...bonusCycles.filter((_, ix) => workers.has(ix)));
        let bestIndex;

        const contractTries: [BladeburnerContractName, number][] = [];
        for (const contract of ns.bladeburner.getContractNames()) {
            const nRemaining = ns.bladeburner.getActionCountRemaining("Contracts", contract);
            contractTries.push([contract, nRemaining]);
        }
        contractTries.sort((a, b) => b[1] - a[1]);
        let bestContract: BladeburnerContractName | undefined = undefined;
        if (contractTries[0][1] > 0 && doContracts) {
            bestContract = contractTries[0][0];
        }

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
                            if (ns.sleeve.getSleeve(i).shock > 0) {
                                ns.sleeve.setToShockRecovery(i);
                            } else {
                                ns.sleeve.setToIdle(i);
                            }
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
                    const time = ns.sleeve.getSleeve(i).storedCycles;
                    if (time / 5 >= 5 * M) {
                        if (ns.sleeve.getSleeve(i).shock > 0) {
                            ns.print(`INFO: Extra stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to Shock Recovery...`);
                            ns.sleeve.setToShockRecovery(i);
                            if (contractIndex == i) {
                                contractIndex = -1;
                            }
                        } else if ((contractIndex == -1 || contractIndex == i) && bestContract !== undefined) {
                            const task = ns.sleeve.getTask(i);
                            if ((task !== null && task.type == "BLADEBURNER" && task.actionType == "Contracts" && task.actionName == bestContract) || ns.sleeve.setToBladeburnerAction(i, "Take on contracts", bestContract)) {
                                ns.print(`INFO: Extra stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to Contracts...`);
                                contractIndex = i;
                            } else {
                                ns.print(`WARNING: Unable to set sleeve ${i} to Contracts!`);
                            }
                        } else {
                            if (ns.bladeburner.getCityChaos(ns.bladeburner.getCity()) > CHAOS_LIMIT) {
                                ns.print(`INFO: Extra stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to Diplomacy...`);
                                ns.sleeve.setToBladeburnerAction(i, "Diplomacy");
                            } else {
                                ns.print(`INFO: Extra stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to idle...`);
                                ns.sleeve.setToIdle(i);
                            }
                            if (contractIndex == i) {
                                contractIndex = -1;
                            }
                        }
                    } else {
                        if (ns.sleeve.getSleeve(i).shock > 0) {
                            ns.print(`INFO: Not enough stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to Shock Recovery...`);
                            ns.sleeve.setToShockRecovery(i);
                        } else {
                            ns.print(`INFO: Not enough stored time (${formatTime(ns.sleeve.getSleeve(i).storedCycles / 5)}) for sleeve ${i}, setting to idle...`);
                            ns.sleeve.setToIdle(i);
                        }
                        if (contractIndex == i) {
                            contractIndex = -1;
                        }
                    }
                }
            }
        }

        if (msToWait == undefined) {
            await ns.sleep(dftWait);
        } else {
            ns.print(`${bold}Infiltration will be done in <=${formatTime(msToWait / 1000)}.${reset}`);
            ns.print(`${bold}Sleeve ${infiltratingIndex} on Infiltration, ${contractIndex} performing Contracts (${bestContract}) ${reset}`);
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
