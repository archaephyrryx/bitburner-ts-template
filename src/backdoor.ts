import { NS } from "@ns";
import { getGraph } from "./census";
import { getRoute } from "./findpath";
import { ScriptFile, getScript } from "./scriptfiles";

/**
 * Times to wait for an external resource or skill level to become sufficient to
 * root a remote host.
 */
export const SleepConstants = {
    script: 1000,
    hackingLevel: 1000,
};

const coreServers = ["CSEC", "avmnite-02h", "I.I.I.I", "run4theh111z"] as const;
const extraServers = ["fulcrumassets", "The-Cave"] as const;


function isBackdoored(ns: NS, serv: string): boolean {
    return ns.getServer(serv).backdoorInstalled ?? false;
}

type PortCount = 0 | 1 | 2 | 3 | 4 | 5;

export function assertPortCount(n: number): asserts n is PortCount {
    if (n > 5 || n < 0) {
        throw new Error(`Unexpected non-portcount number {n}`);
    }
    return;
}

async function spawnJoin(ns: NS, script: string, ...args: string[]): Promise<boolean> {
    const pid = ns.exec(script, "home", {}, ...args);
    if (pid === 0) {
        ns.tprint(`ERROR: Unable to exec '${script} ${args.join(' ')}'!`);
        return false;
    }
    while (ns.isRunning(pid)) {
        await ns.sleep(1000);
    }
    return true;
}

async function execGetScript(ns: NS, scriptfile: `${ScriptFile}`): Promise<boolean> {
    const ret = await spawnJoin(ns, "scriptfiles.js", scriptfile as string);
    if (ret && !ns.fileExists(scriptfile, "home")) {
        ns.tprint(`ERROR: ran script to acquire ${scriptfile}, but process terminated before file was obtained!`);
        return false;
    }
    return ret;
}

async function execGetExploits(ns: NS, portCount: PortCount) {
    if (portCount >= 1 && !await execGetScript(ns, "BruteSSH.exe")) ns.exit();
    if (portCount >= 2 && !await execGetScript(ns, ScriptFile.FTPCrack)) ns.exit();
    if (portCount >= 3 && !await execGetScript(ns, ScriptFile.relaySMTP)) ns.exit();
    if (portCount >= 4 && !await execGetScript(ns, ScriptFile.HTTPWorm)) ns.exit();
    if (portCount >= 5 && !await execGetScript(ns, ScriptFile.SQLInject)) ns.exit();
    return;
}

/**
 * Asynchronous function that waits for all necessary conditions to be met before a server can be
 * rooted, opening ports and nuking once possible. Will then wait for hacking level to reach the
 * requisite level to compromise the server.
 *
 * @param ns {NS} NetScript instance
 * @param serv {string} Name of the server to hack
 * @param ports {number} Number of ports required to hack the server
 */
async function acquirePorts(ns: NS, serv: string): Promise<void> {
    const ports = ns.getServerNumPortsRequired(serv);
    assertPortCount(ports);
    await execGetExploits(ns, ports);
    if (ports == 5) ns.sqlinject(serv);
    if (ports >= 4) ns.httpworm(serv);
    if (ports >= 3) ns.relaysmtp(serv);
    if (ports >= 2) ns.ftpcrack(serv);
    if (ports >= 1) ns.brutessh(serv);
    ns.nuke(serv);
    const minSkill = ns.getServerRequiredHackingLevel(serv);
    while (ns.getHackingLevel() < minSkill) {
        ns.printf("Waiting for hacking skill to reach %d (currently: %d)", minSkill, ns.getHackingLevel());
        await ns.sleep(SleepConstants.hackingLevel);
    }
}

export async function executeBackdoor(ns: NS, server: string): Promise<void> {
    if (isBackdoored(ns, server)) {
        ns.toast(`Server ${server} has already been backdoored...`, "info", 3000);
        return;
        }
    await acquirePorts(ns, server);
    const path = getRoute(ns, server);
    for (const hop of path) {
        ns.singularity.connect(hop);
    }
    await ns.singularity.installBackdoor();
    ns.singularity.connect("home");
    ns.toast(`Sucessfully installed backdoor on server ${server}`, "success", 2000);
}

export async function main(ns: NS): Promise<void> {
    const nextServ = ns.args[0] as typeof coreServers[number] | typeof extraServers[number] | "all" | undefined;

    if (nextServ === "all") {
        const missing = ([...coreServers]).filter((serv) => !isBackdoored(ns, serv));
        if (missing.length > 0) {
            for (const serv of missing) {
                await executeBackdoor(ns, serv);
            }
        } else {
            ns.tprint("No servers remain to backdoor...");
        }
    } else if (nextServ && ns.serverExists(nextServ)) {
        await executeBackdoor(ns, nextServ);
    } else {
        const missing = ([...coreServers]).filter((serv) => !isBackdoored(ns, serv));
        if (missing.length === 0) {
            ns.tprint(`${ns.getScriptName()}: No more core servers left to backdoor, please provide an argument in order to backdoor another server.`)
        } else {
            await executeBackdoor(ns, missing[0]);
        }
    }
}
