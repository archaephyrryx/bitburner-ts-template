import { NS } from '@ns';

const MAX_STAMINA_TGT = 100;
const AUTO_CONTRACT_MIN_CHANCE = 0.95;
const AUTO_OPERATION_MIN_CHANCE = 0.975;
const ATTEMPTS_LWM = 100;
const ATTEMPTS_HWM = 200;
const CHAOS_LIMIT = 10;

export async function main(ns: NS) {
    ns.disableLog('sleep');
    ns.disableLog('getPlayer');
    ns.disableLog('bladeburner.getCurrentAction');
    ns.disableLog('bladeburner.getStamina');
    ns.disableLog('bladeburner.getActionTime');
    ns.disableLog('bladeburner.getActionTime');
    ns.tail();

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
            if (!attemptOperation(ns)) attemptContract(ns);
        }
        await ns.sleep(200);
    }
}

function isFullHP(ns: NS) {
    const { current, max } = ns.getPlayer().hp;
    return current >= max;
}

function doContract(ns: NS, name: string): boolean {
    const [current, max] = ns.bladeburner.getStamina();
    const myAction = ns.bladeburner.getCurrentAction();

    if (ns.bladeburner.getActionEstimatedSuccessChance("Contracts", name)[0] < AUTO_CONTRACT_MIN_CHANCE) {
        if (ns.bladeburner.getActionAutolevel("Contracts", name)) {
            if (ns.bladeburner.getActionCurrentLevel("Contracts", name) > 1) ns.bladeburner.setActionAutolevel("Contracts", name, false);
        }
        while (ns.bladeburner.getActionEstimatedSuccessChance("Contracts", name)[0] < AUTO_CONTRACT_MIN_CHANCE) {
            const curLevel = ns.bladeburner.getActionCurrentLevel("Contracts", name);
            if (curLevel == 1) break;
            ns.bladeburner.setActionLevel("Contracts", name, curLevel - 1);
        }
    } else {
        let bestLevel = ns.bladeburner.getActionCurrentLevel("Contracts", name) + 1;
        while (bestLevel <= ns.bladeburner.getActionMaxLevel("Contracts", name)) {
            ns.bladeburner.setActionLevel("Contracts", name, bestLevel);
            if (ns.bladeburner.getActionEstimatedSuccessChance("Contracts", name)[0] < AUTO_CONTRACT_MIN_CHANCE) {
                if (bestLevel > 1) {
                    bestLevel -= 1;
                }
                ns.bladeburner.setActionLevel("Contracts", name, bestLevel);
                break;
            } else {
                bestLevel += 1;
            }
        }
        if (bestLevel >= ns.bladeburner.getActionMaxLevel("Contracts", name)) {
            ns.bladeburner.setActionAutolevel("Contracts", name, true);
        }
    }

    if (current > max / 2 && ns.bladeburner.getActionEstimatedSuccessChance("Contracts", name)[0] >= AUTO_CONTRACT_MIN_CHANCE) {
        if (myAction.type != "Contract") {
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
        infos.push([...getOperationInfo(ns, opName), opName]);
    }

    if (infos.some(([count, ,]) => count < ATTEMPTS_LWM)) {
        if (!ns.isRunning("sleeve-man.blade.js", "home")) {
            ns.exec("sleeve-man.blade.js", "home");
        }
    } else if (infos.every(([count, ,]) => count > ATTEMPTS_HWM)) {
        ns.scriptKill("sleeve-man.blade.js", "home");
    }

    infos.sort(([levelA, , nameA], [levelB, , nameB]) => ns.bladeburner.getActionRepGain("Operations", nameB, levelB) - ns.bladeburner.getActionRepGain("Operations", nameA, levelA));

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
        infos.push([...getContractInfo(ns, contractName), contractName]);
    }

    infos.sort(([levelA, , nameA], [levelB, , nameB]) => ns.bladeburner.getActionRepGain("Contracts", nameB, levelB) - ns.bladeburner.getActionRepGain("Contracts", nameA, levelA));

    if (infos.some(([count, ,]) => count < ATTEMPTS_LWM)) {
        if (!ns.isRunning("sleeve-man.blade.js", "home")) {
            ns.exec("sleeve-man.blade.js", "home");
        }
    } else if (infos.every(([count, ,]) => count > ATTEMPTS_HWM)) {
        ns.scriptKill("sleeve-man.blade.js", "home");
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

    if (ns.bladeburner.getActionEstimatedSuccessChance("Operations", name)[0] < AUTO_CONTRACT_MIN_CHANCE) {
        if (ns.bladeburner.getActionAutolevel("Operations", name)) {
            if (ns.bladeburner.getActionCurrentLevel("Operations", name) > 1) ns.bladeburner.setActionAutolevel("Operations", name, false);
        }
        while (ns.bladeburner.getActionEstimatedSuccessChance("Operations", name)[0] < AUTO_CONTRACT_MIN_CHANCE) {
            const curLevel = ns.bladeburner.getActionCurrentLevel("Operations", name);
            if (curLevel == 1) break;
            ns.bladeburner.setActionLevel("Operations", name, curLevel - 1);
        }
    } else {
        let bestLevel = ns.bladeburner.getActionCurrentLevel("Operations", name) + 1;
        while (bestLevel <= ns.bladeburner.getActionMaxLevel("Operations", name)) {
            ns.bladeburner.setActionLevel("Operations", name, bestLevel);
            if (ns.bladeburner.getActionEstimatedSuccessChance("Operations", name)[0] < AUTO_CONTRACT_MIN_CHANCE) {
                if (bestLevel > 1) {
                    bestLevel -= 1;
                }
                ns.bladeburner.setActionLevel("Operations", name, bestLevel);
                break;
            } else {
                bestLevel += 1;
            }
        }
        if (bestLevel >= ns.bladeburner.getActionMaxLevel("Operations", name)) {
            ns.bladeburner.setActionAutolevel("Operations", name, true);
        }
    }

    if (current > max / 2 && ns.bladeburner.getActionEstimatedSuccessChance("Operations", name)[0] >= AUTO_OPERATION_MIN_CHANCE) {
        if (myAction.type != "Operation") {
            if (ns.bladeburner.getActionMaxLevel("Operations", name) == ns.bladeburner.getActionCurrentLevel("Operations", name)) ns.bladeburner.setActionAutolevel("Operations", name, true);
            return ns.bladeburner.startAction("Operations", name);
        }
        else ns.print(`INFO: Will continue action '${name}' (Operations) after finishing current cycle...`);
        return true;
    } else {
        return false;
    }
}
