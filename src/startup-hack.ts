import { NS } from "@ns";
import { nodes, NodeInfo } from 'global';

let defaultTarget = "n00dles";

export function canHack(ns: NS, serverInfo: NodeInfo): boolean {
    const requiredSkill = serverInfo.skill;
    const threshold = ns.getHackingLevel() / 2;
    if (requiredSkill > threshold) {
        return false;
    }
    const portsNeeded = serverInfo.ports;
    if (portsNeeded >= 5 && !ns.fileExists("SQLInject.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 4 && !ns.fileExists("HTTPWorm.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 3 && !ns.fileExists("relaySMTP.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 2 && !ns.fileExists("FTPCrack.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 1 && !ns.fileExists("BruteSSH.exe", "home")) {
        return false;
    }
    return true;
}

export function isProfitable(ns: NS, server: string): boolean {
    if (ns.getServerMaxMoney(server) <= 0) {
        return false;
    }
    return true;
}

export async function main(ns: NS): Promise<void> {
    function setTarget(): string {
        let bestFit = "n00dles";
        for (const servInfo of nodes) {
            if (canHack(ns, servInfo)) {
                if (isProfitable(ns, servInfo.name)) {
                    bestFit = servInfo.name;
                } else {
                    continue;
                }
            } else {
                break;
            }
        }
        defaultTarget = bestFit;
        return defaultTarget;
    }

    const server: string = (ns.args[0] as string ?? setTarget());


    // Defines how much money a server should have before we hack it
    // In this case, it is set to 75% of the server's max money
    const moneyThresh = ns.getServerMaxMoney(server) * 0.75;

    // Defines the maximum security level the target server can
    // have. If the target's security level is higher than this,
    // we'll weaken it before doing anything else
    const securityThresh = ns.getServerMinSecurityLevel(server) + 5;

    if (ns.fileExists("SQLInject.exe", "home")) ns.sqlinject(server);
    if (ns.fileExists("HTTPWorm.exe", "home")) ns.httpworm(server);
    if (ns.fileExists("relaySMTP.exe", "home")) ns.relaysmtp(server);
    if (ns.fileExists("FTPCrack.exe", "home")) ns.ftpcrack(server);
    if (ns.fileExists("BruteSSH.exe", "home")) ns.brutessh(server);
    ns.nuke(server);

    // Infinite loop that continously hacks/grows/weakens the target server
    for (; ;) {
        if (ns.getServerSecurityLevel(server) > securityThresh) {
            // If the server's security level is above our threshold, weaken it
            await ns.weaken(server);
        } else if (ns.getServerMoneyAvailable(server) < moneyThresh) {
            // If the server's money is less than our threshold, grow it
            await ns.grow(server);
        } else {
            // Otherwise, hack it
            await ns.hack(server);
        }
    }
}