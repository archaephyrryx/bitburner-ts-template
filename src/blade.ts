import { AutocompleteData, NS } from '@ns';
import { BladeSnap, formatSuccesses, getSnapshot, successesSinceInstall } from './blade.snapshot';
import { formatTime } from './helper';

const MAX_STAMINA_TGT = 50;
const AUTO_CONTRACT_MIN_CHANCE = 0.95;
const AUTO_OPERATION_MIN_CHANCE = 0.975;
const AUTO_BLACKOPS_MIN_CHANCE = 0.99;
const ATTEMPTS_LWM = 100;
const ATTEMPTS_HWM = 200;
export const CHAOS_LIMIT = 10;
const CONTRACT_OPERATION_RATIO = 0;


export async function main(ns: NS) {
    ns.disableLog('sleep');
    ns.disableLog('getPlayer');
    ns.disableLog('bladeburner.getCurrentAction');
    ns.disableLog('bladeburner.getStamina');
    ns.disableLog('bladeburner.getActionTime');
    ns.disableLog('bladeburner.getActionTime');
    ns.tail();

    const snapshot = getSnapshot(ns);

    for (; ;) {
        ns.clearLog();
        const [current, max] = ns.bladeburner.getStamina();
        ns.print(`Stamina: ${ns.formatNumber(current)} / ${ns.formatNumber(max)}`)

        const myAction = ns.bladeburner.getCurrentAction();
        ns.print(`Current: ${myAction.name} (${myAction.type})`);


        if (!isFullHP(ns)) {
            if (myAction.type != "General" || myAction.name != "Hyperbolic Regeneration Chamber") ns.bladeburner.startAction("General", "Hyperbolic Regeneration Chamber");
            else ns.print(`INFO: Will continue action 'Hyperbolic Regeneration Chamber' (General) after finishing current cycle...`);
        } else if (max < MAX_STAMINA_TGT) {
            if (myAction.type == "General" && myAction.name == "Training") {
                ns.print(`INFO: Will continue action 'Training' (General) after finishing current cycle...`);
            } else {
                ns.bladeburner.startAction("General", "Training");
            }
        } else {
            if (!attemptBlackOp(ns)) {
                if (contractSwitch(ns, snapshot) || !attemptOperation(ns)) attemptContract(ns);
            }
        }
        await ns.sleep(200);
    }
}



function contractSwitch(ns: NS, snapshot: BladeSnap): boolean {
    const [contCount, opCount] = successesSinceInstall(ns, snapshot);
    ns.print(`INFO: ${formatSuccesses(contCount, opCount)}`);
    return (contCount === 0 || (opCount != 0 && contCount <= CONTRACT_OPERATION_RATIO * opCount));
}


function isFullHP(ns: NS) {
    const { current, max } = ns.getPlayer().hp;
    return current >= max;
}

function adjustLevel(ns: NS, type: string, name: string, minChance: number) {
    if (ns.bladeburner.getActionEstimatedSuccessChance(type, name)[0] < minChance) {
        if (ns.bladeburner.getActionAutolevel(type, name)) {
            if (ns.bladeburner.getActionCurrentLevel(type, name) > 1) ns.bladeburner.setActionAutolevel(type, name, false);
        }
        while (ns.bladeburner.getActionEstimatedSuccessChance(type, name)[0] < minChance) {
            const curLevel = ns.bladeburner.getActionCurrentLevel(type, name);
            if (curLevel == 1) break;
            ns.bladeburner.setActionLevel(type, name, curLevel - 1);
        }
    } else {
        let bestLevel = ns.bladeburner.getActionCurrentLevel(type, name) + 1;
        while (bestLevel <= ns.bladeburner.getActionMaxLevel(type, name)) {
            ns.bladeburner.setActionLevel(type, name, bestLevel);
            if (ns.bladeburner.getActionEstimatedSuccessChance(type, name)[0] < minChance) {
                if (bestLevel > 1) {
                    bestLevel -= 1;
                }
                ns.bladeburner.setActionLevel(type, name, bestLevel);
                break;
            } else {
                bestLevel += 1;
            }
        }
        if (bestLevel >= ns.bladeburner.getActionMaxLevel(type, name)) {
            ns.bladeburner.setActionAutolevel(type, name, true);
        }
    }
}

function doContract(ns: NS, name: string): boolean {
    const [current, max] = ns.bladeburner.getStamina();
    const myAction = ns.bladeburner.getCurrentAction();


    if (current > max / 2 && ns.bladeburner.getActionEstimatedSuccessChance("Contracts", name)[0] >= AUTO_CONTRACT_MIN_CHANCE) {
        if (myAction.type != "Contract" || myAction.name != name) {
            if (ns.bladeburner.getActionMaxLevel("Contracts", name) == ns.bladeburner.getActionCurrentLevel("Contracts", name)) ns.bladeburner.setActionAutolevel("Contracts", name, true);
            return ns.bladeburner.startAction("Contracts", name);
        }
        else ns.print(`INFO: Will continue action '${name}' (Contracts) after finishing current cycle...`);
        return true;
    } else {
        return false;
    }
}

function attemptOperation(ns: NS) {
    const infos: [number, number, string][] = [];
    const operations = ns.bladeburner.getOperationNames();
    for (const opName of operations) {
        if (opName == "Sting Operation" || opName == "Raid" || opName == "Stealth Retirement Operation") continue;
        adjustLevel(ns, "Operations", opName, AUTO_OPERATION_MIN_CHANCE);
        infos.push([...getOperationInfo(ns, opName), opName]);
    }

    if (infos.some(([count, ,]) => count < ATTEMPTS_LWM)) {
        if (!ns.isRunning("sleeve-man.blade.js", "home") && !ns.isRunning("sleeve-man.blade.js", "home", "--doContracts")) {
            ns.exec("sleeve-man.blade.js", "home");
        }
    } else if (infos.every(([count, ,]) => count > ATTEMPTS_HWM)) {
        // ns.scriptKill("sleeve-man.blade.js", "home");
    }
    infos.sort(([, levelA, nameA], [, levelB, nameB]) => operationYield(ns, nameB, levelB) - operationYield(ns, nameA, levelA));

    for (const [, , name] of infos) {
        if (doOperation(ns, name)) {
            return true;
        }
    }

    return false;
}

function attemptContract(ns: NS) {
    const myAction = ns.bladeburner.getCurrentAction();

    const infos: [number, number, string][] = [];
    for (const contractName of ns.bladeburner.getContractNames()) {
        adjustLevel(ns, "Contracts", contractName, AUTO_CONTRACT_MIN_CHANCE);
        infos.push([...getContractInfo(ns, contractName), contractName]);
    }

    infos.sort(([, levelA,], [, levelB,]) => levelA - levelB);

    if (infos.some(([count, ,]) => count < ATTEMPTS_LWM)) {
        if (!ns.isRunning("sleeve-man.blade.js", "home")) {
            ns.exec("sleeve-man.blade.js", "home");
        }
    } else if (infos.every(([count, ,]) => count > ATTEMPTS_HWM)) {
        // ns.scriptKill("sleeve-man.blade.js", "home");
    }

    for (const [, , name] of infos) {
        if (doContract(ns, name)) {
            return;
        }
    }

    if (ns.bladeburner.getCityChaos(ns.bladeburner.getCity()) > CHAOS_LIMIT) {
        if (myAction.type != "General" || myAction.name != "Diplomacy") ns.bladeburner.startAction("General", "Diplomacy");
        else ns.print(`INFO: Will continue action 'Diplomacy' (General) after finishing current cycle...`);
    } else {
        if (myAction.type != "General" || myAction.name != "Field Analysis") ns.bladeburner.startAction("General", "Field Analysis");
        else ns.print(`INFO: Will continue action 'Field Analysis' (General) after finishing current cycle...`);
    }
}

function getContractInfo(ns: NS, contractName: string): [number, number] {
    const count = ns.bladeburner.getActionCountRemaining("Contracts", contractName);
    const maxLevel = ns.bladeburner.getActionMaxLevel("Contracts", contractName);

    return [count, maxLevel];
}

type OperationInfo = [number, number];

function getOperationInfo(ns: NS, opName: string): OperationInfo {
    const opAttempts = ns.bladeburner.getActionCountRemaining("Operations", opName);
    const opLevel = ns.bladeburner.getActionCurrentLevel("Operations", opName);
    return [opAttempts, opLevel];
}

function doOperation(ns: NS, name: string) {
    const [current, max] = ns.bladeburner.getStamina();
    const myAction = ns.bladeburner.getCurrentAction();

    if (ns.bladeburner.getActionCountRemaining("Operations", name) < 1) {
        return false;
    }

    if (current > max / 2 && ns.bladeburner.getActionEstimatedSuccessChance("Operations", name)[0] >= AUTO_OPERATION_MIN_CHANCE) {
        if (myAction.type != "Operation" || myAction.name != name) {
            if (ns.bladeburner.getActionMaxLevel("Operations", name) == ns.bladeburner.getActionCurrentLevel("Operations", name)) ns.bladeburner.setActionAutolevel("Operations", name, true);
            return ns.bladeburner.startAction("Operations", name);
        }
        else ns.print(`INFO: Will continue action '${name}' (Operations) after finishing current cycle...`);
        return true;
    } else {
        return false;
    }
}

function operationYield(ns: NS, name: string, level: number): number {
    return ns.bladeburner.getActionRepGain("Operations", name, level) / ns.bladeburner.getActionTime("Operations", name);
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    return [];
}

function attemptBlackOp(ns: NS): boolean {
    const nextOp = ns.bladeburner.getNextBlackOp();
    if (nextOp == null || nextOp.rank > ns.bladeburner.getRank()) return false;

    const [minChance, maxChance] = ns.bladeburner.getActionEstimatedSuccessChance("BlackOps", nextOp.name);
    if (minChance < AUTO_BLACKOPS_MIN_CHANCE) {
        if (minChance < maxChance) {
            const myAction = ns.bladeburner.getCurrentAction();
            if (myAction.type != "General" || myAction.name != "Field Analysis") ns.bladeburner.startAction("General", "Field Analysis");
            else ns.print(`INFO: Will continue action 'Field Analysis' (General) after finishing current cycle...`);
            return true;
        }
        return false;
    } else {
        const myAction = ns.bladeburner.getCurrentAction();
        if (myAction.type != "BlackOp") {
            ns.bladeburner.startAction("BlackOps", nextOp.name);
        }
        const timeLeft = ns.bladeburner.getActionTime("BlackOps", nextOp.name) - ns.bladeburner.getActionCurrentTime();
        ns.print(`INFO: Attempting Black Op '${nextOp.name}': ${ns.formatPercent(minChance)} ~ ${ns.formatPercent(maxChance)} success rate, ${formatTime(Math.floor(timeLeft / 1000))} left.`);
        return true;
    }
}
