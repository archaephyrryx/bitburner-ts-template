import { NS } from "@ns";
import { nodes } from "./global";

type RamInfo = { home: number, purchased: number, rooted: number };


function usedRam(ns: NS): RamInfo {
    const totalRam = { home: ns.getServerMaxRam("home"), purchased: 0, rooted: 0 };
    for (const server of ns.getPurchasedServers()) {
        totalRam.purchased += ns.getServerUsedRam(server);
    }
    for (const node of nodes) {
        if (ns.hasRootAccess(node.name)) {
            totalRam.rooted += ns.getServerUsedRam(node.name);
        }
    }
    return totalRam;
}

function controlledRam(ns: NS): RamInfo {
    const totalRam = { home: ns.getServerMaxRam("home"), purchased: 0, rooted: 0 };
    for (const server of ns.getPurchasedServers()) {
        totalRam.purchased += ns.getServerMaxRam(server);
    }
    for (const node of nodes) {
        if (ns.hasRootAccess(node.name)) {
            totalRam.rooted += ns.getServerMaxRam(node.name);
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


export async function main(ns: NS): Promise<void> {
    const avail = controlledRam(ns);
    const used = usedRam(ns);
    const free = diffRam(avail, used);
    ns.tprint(`Controlled RAM: ${formatInfo(ns, avail)}`);
    ns.tprint(`Used RAM: ${formatInfo(ns, used)}`);
    ns.tprint(`Free RAM: ${formatInfo(ns, free)}`);
}