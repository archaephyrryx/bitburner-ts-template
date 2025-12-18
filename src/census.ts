import { NS } from "@ns";
import { NodeInfo } from "./global";
import { explore } from "./graph";

export type Graph = { populated: false } | { populated: true, nodes: NodeInfo[] }
export let graph: Graph = { populated: false };

export function getGraph(ns: NS, force = false): NodeInfo[] {
    if (!graph.populated || force) {
        graph = { populated: true, nodes: explore(ns) };
    }
    return graph.nodes;
}

type RamInfo = { home: number, purchased: number, rooted: number };

function usedRam(ns: NS): RamInfo {
    const totalRam = { home: 0, purchased: 0, rooted: 0 };
    const servers = getGraph(ns);
    for (const server of servers) {
        if (ns.hasRootAccess(server.name)) {
            const usedRam = ns.getServerUsedRam(server.name);
            if (server.name === "home") {
                totalRam.home += usedRam;
            } else if (server.name.startsWith("pserv") || server.name.startsWith("hacknet-server")) {
                totalRam.purchased += usedRam;
            } else {
                totalRam.rooted += usedRam;
            }
        }
    }
    return totalRam;
}

function controlledRam(ns: NS): RamInfo {
    const totalRam = { home: 0, purchased: 0, rooted: 0 };
    const servers = getGraph(ns);
    for (const server of servers) {
        if (ns.hasRootAccess(server.name)) {
            const maxRam = ns.getServerMaxRam(server.name);
            if (server.name === "home") {
                totalRam.home += maxRam;
            } else if (server.name.startsWith("pserv") || server.name.startsWith("hacknet-server")) {
                totalRam.purchased += maxRam;
            } else {
                totalRam.rooted += maxRam;
            }
        }
    }
    return totalRam;
}

function diffRam(x: RamInfo, y: RamInfo): RamInfo {
    return {
        home: x.home - y.home,
        purchased: x.purchased - y.purchased,
        rooted: x.rooted - y.rooted,
    };
}

function formatInfo(ns: NS, info: RamInfo): string {
    const total = info.home + info.purchased + info.rooted;
    return `${ns.formatRam(total)} (${ns.formatRam(info.home)}|${ns.formatRam(info.purchased)}|${ns.formatRam(info.rooted)})`;
}

export function displayRam(ns: NS) {
    const avail = controlledRam(ns);
    const used = usedRam(ns);
    const free = diffRam(avail, used);
    ns.tprint(`Controlled RAM: ${formatInfo(ns, avail)}`);
    ns.tprint(`Used RAM: ${formatInfo(ns, used)}`);
    ns.tprint(`Free RAM: ${formatInfo(ns, free)}`);
}


export async function main(ns: NS): Promise<void> {
    displayRam(ns);
    return;
}
