import { NS } from "@ns";
import { getGraph } from 'census';

export enum SelfObjectives {
    GetScripts = "Get Scripts",
    JoinCoreFactions = "Join Core Factions",
    ObtainTIXAPI = "Obtain TIX API",
    AugmentHome = "Augment Home",
    GrindMegacorp = "Grind Megacorp Reputation",
}

export enum SleeveObjectives {
    ZeroShock = "Zero Shock",
    PurchaseAugments = "Purchase Augments",
}

export async function autoCrack(ns: NS, server: string, portsHint?: number): Promise<void> {
    if (!(server === "home" || ns.hasRootAccess(server))) {
        ns.print(`Cracking ${server}...`)
        const portsNeeded = portsHint ?? ns.getServerNumPortsRequired(server);
        if (portsNeeded >= 5) {
            while (!ns.fileExists("SQLInject.exe", "home")) {
                await ns.sleep(60000);
            }
            ns.sqlinject(server);
        }
        if (portsNeeded >= 4) {
            while (!ns.fileExists("HTTPWorm.exe", "home")) {
                await ns.sleep(60000);
            }
            ns.httpworm(server);
        }
        if (portsNeeded >= 3) {
            while (!ns.fileExists("relaySMTP.exe", "home")) {
                await ns.sleep(60000);
            }
            ns.relaysmtp(server);
        }
        if (portsNeeded >= 2) {
            while (!ns.fileExists("FTPCrack.exe", "home")) {
                await ns.sleep(60000);
            }
            ns.ftpcrack(server);
        }
        if (portsNeeded >= 1) {
            while (!ns.fileExists("BruteSSH.exe", "home")) {
                await ns.sleep(60000);
            }
            ns.brutessh(server);
        }
        ns.nuke(server);
    }
}

/** @param ns {NS}
 * @param scriptName {string}
 * @param args {string[]}
 */
export function runOnce(ns: NS, scriptName: string, ...args: string[]) {
    if (ns.isRunning(scriptName, "home", ...args)) {
        return;
    }
    ns.exec(scriptName, "home", {}, ...args)
}


export async function main(ns: NS): Promise<void> {
    // Copy our scripts onto each server that requires 0 ports
    // to gain root access. Then use nuke() to gain admin access and
    // run the scripts.
    ns.disableLog("getHackingLevel");
    ns.disableLog("sleep");

    runOnce(ns, "dispatch.js");
    runOnce(ns, "monitor.js");
    if (ns.stock.has4SDataTIXAPI()) {
        runOnce(ns, "market.js", "autotrade");
    }
    runOnce(ns, "custom-stats.js");
    runOnce(ns, "hackNet.js", "init");
    runOnce(ns, "server.js", "init");
    runOnce(ns, "greensleeves.js");
    runOnce(ns, "backdoor.js", "all");
    runOnce(ns, "jobber.js");
    runOnce(ns, "budget.js");
    runOnce(ns, "factoid.js", "--verbose");
    runOnce(ns, "bong.js");
    runOnce(ns, "kawaii.js", "--focus", "money", "--equip");

    const orderedNodes = []
    const nodes = getGraph(ns, true);

    for (const servInfo of nodes) {
        const serv = servInfo.name;
        const actualSkill = ns.getServerRequiredHackingLevel(serv);
        if (actualSkill !== servInfo.skill) {
            ns.tprintf("INFO: Global server table reports %s requires %d hacking skill, but actual skill requirement is %d", serv, servInfo.skill, actualSkill);
            ns.tprintf(`HINT: { name: "${servInfo.name}", skill: ${actualSkill}, ports: ${servInfo.ports} }`, serv, servInfo.skill, actualSkill);
        }
        orderedNodes.push({ ...servInfo, skill: actualSkill });
    }

    orderedNodes.sort((a, b) => a.ports < b.ports ? -1 : a.ports > b.ports ? 1 : a.skill - b.skill);

    for (const servInfo of orderedNodes) {
        let lastHackingLevel = ns.getHackingLevel();
        while (lastHackingLevel < servInfo.skill) {
            const currentHackingLevel = ns.getHackingLevel();
            if (currentHackingLevel > lastHackingLevel) {
                ns.printf("Current hacking skill %d insufficient (<%d) to hack %s...", currentHackingLevel, servInfo.skill, servInfo.name);
                lastHackingLevel = currentHackingLevel;
            }
            await ns.sleep(1000);
        }
        await autoCrack(ns, servInfo.name, servInfo.ports);
    }
    return;
}
