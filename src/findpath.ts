import { AutocompleteData, NS } from "@ns";
import { recursiveScan } from "./find_server";

function search(ns: NS, target: string): string[] {
    const route: string[] = [];
    recursiveScan(ns, '', "home", target, route);
    return route;
}

export function shortCircuit(ns: NS, path: string[]): string[] {
    const revPath = new Array(...path).reverse();
    const [dest, ...links] = revPath;
    if (links.length > 0) {
        const canConnect = (link: string): boolean => ns.getServer(link).backdoorInstalled ?? false;
        const index = links.findIndex(canConnect);
        if (index >= 0) {
            const shortPath = links.slice(0, index + 1);
            const ret = new Array(...shortPath).reverse();
            ret.push(dest);
            return ret;
        }
    }
    return path;
}

export async function main(ns: NS): Promise<void> {
    const [target, ...command] = ns.args as string[];
    if (!ns.serverExists(target)) {
        ns.tprintf("ERROR: %s does not exist", target);
        return;
    }
    const route = search(ns, target);
    const newRoute = shortCircuit(ns, route);

    let message = newRoute.map((node) => "connect " + node).join(" ; ");
    if (command.length > 0) {
        message += ` ; ${command.join(' ')}`;
    }
    ns.tprint(message);
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return [...data.servers];
}
