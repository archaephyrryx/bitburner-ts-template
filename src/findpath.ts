import { AutocompleteData, NS } from "@ns";
import { recursiveScan } from "./find_server";

function search(ns: NS, target: string): string[] {
    const route: string[] = [];
    recursiveScan(ns, '', "home", target, route);
    return route;
}

export async function main(ns: NS): Promise<void> {
    const target = ns.args[0] as string;
    if (!ns.serverExists(target)) {
        ns.tprintf("ERROR: %s does not exist", target);
        return;
    }
    const route = search(ns, target);

    ns.tprint(route.map((node) => "connect " + node).join(" ; "));
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return [...data.servers];
}