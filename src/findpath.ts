import { AutocompleteData, NS } from "@ns";

async function search(ns: NS, target: string, route: string[] = []): Promise<string[]> {
    const routePrefix: string[] = [...route];
    if (routePrefix.length === 0) {
        routePrefix.push("home");
    }
    let ret: string[] = [];
    const neighbors = ns.scan(route[route.length - 1]);
    for (const node of neighbors) {
        if (node === target) {
            routePrefix.push(node);
            return routePrefix;
        }
        if (!routePrefix.includes(node)) {
            ret = await search(ns, target, [...routePrefix, node]);
            if (ret.length > 0) {
                return ret;
            }
        }
        await ns.sleep(100);
    }
    return ret;
}

export async function main(ns: NS): Promise<void> {
    const target = ns.args[0] as string;
    if (!ns.serverExists(target)) {
        ns.tprintf("ERROR: %s does not exist", target);
        return;
    }
    const route = await search(ns, target, []);

    ns.tprint(route.map((node) => "connect " + node).join(" ; "));
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return [...data.servers];
}