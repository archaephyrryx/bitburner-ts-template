import { NS } from "@ns";
import { getGraph } from "./census";

const serverOrder = ["CSEC", "avmnite-02h", "I.I.I.I", "run4theh111z"] as const;

export const backdoors: { [key: string]: number } = {
    "CSEC": 1,
    "avmnite-02h": 2,
    "I.I.I.I": 3,
    "run4theh111z": 4,
};

function isBackdoored(ns: NS, serv: string): boolean {
    return ns.getServer(serv).backdoorInstalled ?? false;
}

async function missingBackdoors(ns: NS): Promise<string[]> {
    const ret = [];
    const nodes = getGraph(ns, true);
    for (const node of nodes) {
        const name = node.name;
        if (name === "home" || name.startsWith("pserv")) {
            continue;
        }
        if (!isBackdoored(ns, name)) {
            ret.push(name);
        }
    }
    return ret;
}

async function acquirePorts(ns: NS, serv: string, ports: number): Promise<void> {
    if (ports == 5) {
        while (!ns.fileExists("SQLInject.exe", "home")) {
            await ns.sleep(60000);
        }
        ns.sqlinject(serv);
    }
    if (ports >= 4) {
        while (!ns.fileExists("HTTPWorm.exe", "home")) {
            await ns.sleep(60000);
        }
        ns.httpworm(serv);
    }
    if (ports >= 3) {

        while (!ns.fileExists("RelaySMTP.exe", "home")) {
            await ns.sleep(60000);
        }
        ns.relaysmtp(serv);
    }
    if (ports >= 2) {
        while (!ns.fileExists("FTPCrack.exe", "home")) {
            await ns.sleep(60000);
        }
        ns.ftpcrack(serv);
    }
    if (ports >= 1) {
        while (!ns.fileExists("BruteSSH.exe", "home")) {
            await ns.sleep(60000);
        }
        ns.brutessh(serv);
    }
    ns.nuke(serv);
    const minSkill = ns.getServerRequiredHackingLevel(serv);
    while (ns.getHackingLevel() < minSkill) {
        ns.printf("Waiting for hacking skill to reach %d (currently: %d)", minSkill, ns.getHackingLevel());
        await ns.sleep(60000);
    }
}

export async function main(ns: NS): Promise<void> {
    async function capture(serv: string): Promise<boolean> {
        if (ns.hasRootAccess(serv)) {
            return false;
        } else {
            await acquirePorts(ns, serv, backdoors[serv]);
            return true;
        }
    }
    async function improvise(): Promise<string | undefined> {
        for (const serv in backdoors) {
            if (await capture(serv)) {
                return serv;
            } else {
                ns.tprint("Already captured " + serv);
                continue;
            }
        }
        return
    }

    const nextServ = ns.args[0] as typeof serverOrder[number] | "all" | undefined;
    if (nextServ === "all") {
        const rem = await missingBackdoors(ns);
        if (rem.length > 0) {
            for (const serv of rem) {
                ns.tprint(`Potential backdoor available on server: ${serv}`);
            }
        } else {
            ns.tprint("No servers remain to backdoor...");
        }
    } else if (nextServ && ns.serverExists(nextServ)) {
        await capture(nextServ);
        ns.toast("Backdoor preparations on " + nextServ + " complete", "success", 2000);
    } else {
        const res = await improvise();
        const msg = (res ?? "All recognized honeypots") + " captured";
        ns.toast(msg, "success", 2000);
    }
}
