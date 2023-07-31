import { NS } from "@ns";
import { nodes } from 'global';
import { idealThreads } from "./helper";

export async function autohack(ns: NS, server: string, portsHint?: number, renew = false): Promise<number> {
    let retval = 0;
    if (!(server === "home" || ns.hasRootAccess(server))) {
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
        retval = 1;
    }
    if (renew) {
        ns.scriptKill("startup-hack.js", server);
    }
    const res = idealThreads(ns, "startup-hack.js", server, ["global.js"]);
    if (res.canRun) {
        ns.printf("startup-hack.js can run %d threads on %s", res.threads, server);
        if (res.threads === 0) {
            if (ns.isRunning("startup-hack.js", server)) {
                ns.printf("startup-hack.js is already running on %s", server);
                retval = 1;
            } else {
                ns.printf("startup-hack.js is not running on %s", server);
                retval = 0;
            }
        } else {
            const pid = ns.exec("startup-hack.js", server, { threads: res.threads });
            if (pid === 0) {
                ns.toast(`Failed to run startup-hack.js on ${server}`, "warning", 1500);
                ns.tprintf("autohack failed, but you can try to manually run `connect %s` >> `run startup-hack.js -t %d`", server, res);
                retval = -1;
            } else {
                ns.toast(`Running ${res.threads} threads of startup-hack.js on ${server}`, "info", 1500);
                retval = 1;
            }
        }
    } else {
        ns.toast(`Unable to run startup-hack.js on ${server}`, "warning", 1500);
        retval = -1;
    }
    return retval;
}

export async function main(ns: NS): Promise<void> {
    // Copy our scripts onto each server that requires 0 ports
    // to gain root access. Then use nuke() to gain admin access and
    // run the scripts.
    ns.disableLog("getHackingLevel");
    ns.disableLog("sleep");
    const renew = (ns.args[0] === "renew");

    for (const servInfo of nodes) {
        const serv = servInfo.name;
        const actualSkill = ns.getServerRequiredHackingLevel(serv);
        if (actualSkill !== servInfo.skill) {
            ns.tprintf("INFO: Global server table reports %s requires %d hacking skill, but actual skill requirement is %d", serv, servInfo.skill, actualSkill);
            ns.tprintf(`HINT: { name: "${servInfo.name}", skill: ${actualSkill}, ports: ${servInfo.ports} }`, serv, servInfo.skill, actualSkill);
        }
        let lastHackingLevel = ns.getHackingLevel();
        while (lastHackingLevel < actualSkill) {
            const currentHackingLevel = ns.getHackingLevel();
            if (currentHackingLevel > lastHackingLevel) {
                ns.printf("Current hacking skill %d insufficient (<%d) to hack %s...", currentHackingLevel, actualSkill, servInfo.name);
                lastHackingLevel = currentHackingLevel;
            }
            await ns.sleep(1000);
        }
        ns.tprintf("running autohack on server %s", serv);
        const res = await autohack(ns, serv, servInfo.ports, renew);
        switch (res) {
            case -1:
                ns.toast("Failed to run autohack on " + serv, "warning", 1500);
                break;
            case 0:
                break;
            case 1:
                ns.toast("Acquired control of " + serv, "success", 1500);
                break;
            default:
                throw new Error("unreachable!");
        }
    }
    return;
}