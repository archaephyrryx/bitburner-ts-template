import { NS } from "@ns";

const serverOrder = ["CSEC", "avmnite-02h", "I.I.I.I"] as const;

export const backdoors: { [key: string]: number } = {
    "CSEC": 1,
    "avmnite-02h": 2,
    "I.I.I.I": 3,
};

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

    const nextServ = ns.args[0] as typeof serverOrder[number] | undefined;
    if (nextServ && ns.serverExists(nextServ)) {
        await capture(nextServ);
        ns.toast("Backdoor preparations on " + nextServ + " complete", "success", 2000);
    } else {
        const res = await improvise();
        const msg = (res ?? "All recognized honeypots") + " captured";
        ns.toast(msg, "success", 2000);
    }
}