import { AutocompleteData, NS } from '@ns';
import { getRoute } from './findpath';

export async function main(ns: NS) {
    const target = ns.args[0];
    if (typeof target === 'string') {
        const path = getRoute(ns, target);
        for (const hop of path) {
            if (!ns.singularity.connect(hop)) {
                ns.tprint(`ERROR: unable to connect to ${hop} (en route -*-> ${target})`);
                ns.exit();
            }
        }
    }
}

export function autocomplete(data: AutocompleteData, ...args: string[]) {
    return data.servers;
}
