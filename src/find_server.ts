import { NS, AutocompleteData } from "@ns";

export function recursiveScan(ns: NS, parent: string, server: string, target: string, route: string[]): boolean {
    const children = ns.scan(server);
    for (const child of children) {
        if (parent == child) {
            continue;
        }
        if (child == target) {
            route.unshift(child);
            route.unshift(server);
            return true;
        }

        if (recursiveScan(ns, server, child, target, route)) {
            route.unshift(server);
            return true;
        }
    }
    return false;
}

export async function main(ns: NS) {
    const args = ns.flags([["help", false]]);
    const route: string[] = [];
    const server = Array.isArray(args._) ? args._[0] : "";
    if (!server || args.help) {
        ns.tprint("This script helps you find a server on the network and shows you the path to get to it.");
        ns.tprint(`Usage: run ${ns.getScriptName()} SERVER`);
        ns.tprint("Example:");
        ns.tprint(`> run ${ns.getScriptName()} n00dles`);
        return;
    }

    recursiveScan(ns, '', 'home', server, route);
    for (let i = 0; i < route.length; i++) {
        await ns.sleep(500);
        const extra = (i > 0) ? "└ " : "";
        ns.tprint(`${" ".repeat(i)}${extra}${route[i]}`);
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return [...data.servers];
}