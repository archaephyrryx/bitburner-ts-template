import { NS } from "@ns";
import { NodeInfo } from "./global";

export function getInfo(ns: NS, name: string): NodeInfo {
    const skill = ns.getServerRequiredHackingLevel(name);
    const ports = ns.getServerNumPortsRequired(name);
    return { name, skill, ports };
}

export function explore(ns: NS): NodeInfo[] {
    const info: NodeInfo[] = [];
    const seen: string[] = [];
    let frontier = ns.scan("home");
    while (frontier.length > 0) {
        const node = frontier.pop() as string;
        if (seen.includes(node)) {
            continue;
        }
        const nodeInfo = getInfo(ns, node);
        info.push(nodeInfo);
        seen.push(node);
        const next = ns.scan(node);
        frontier = frontier.concat(next);
    }
    info.sort((a, b) => (a.name == "home") ? -1 : (a.ports > b.ports) ? 1 : (a.ports < b.ports) ? -1 : (a.skill - b.skill));
    return info;
}

export async function main(ns: NS): Promise<void> {
    const nodes = explore(ns);
    ns.tprint(nodes);
}

