import { AutocompleteData, NS, ScriptArg } from '@ns';

type OtherSources = { kind: "exclusive", graftable: boolean } | { kind: "factions", numberJoined: number, otherFactions: string[] };

type AugReq = { augName: string, otherSources: OtherSources, reqRep: number };

export type FactionRepProgress = { factionName: string, augReqs: AugReq[], myRep: number };

const Flags: [string, (number | boolean)][] =
    [
        ['refreshRate', 1000],
        ['verbose', false],
    ];

export async function main(ns: NS) {
    const flags = ns.flags(Flags);
    ns.tail();
    ns.disableLog('ALL');

    for (; ;) {
        ns.clearLog();
        const table = getFactionRepProgress(ns);
        display(ns, table, flags);
        await ns.sleep(flags.refreshRate as ScriptArg as number);
    }
}

export function getFactionRepProgress(ns: NS): FactionRepProgress[] {
    const factions = ns.getPlayer().factions;
    const ownedAugs = ns.singularity.getOwnedAugmentations(true);
    const graftableAugs = ns.grafting.getGraftableAugmentations();
    const table: FactionRepProgress[] = [];
    outer: for (const factionName of factions) {
        const allAugs = ns.singularity.getAugmentationsFromFaction(factionName);
        const missing = allAugs.filter((a) => !ownedAugs.includes(a));
        const augReqs: AugReq[] = [];
        const myRep = ns.singularity.getFactionRep(factionName);
        if (missing.length === 0) {
            table.push({ factionName, augReqs, myRep });
            continue outer;
        }
        inner: for (const augName of missing) {
            const reqRep = ns.singularity.getAugmentationRepReq(augName);
            const allFactions = ns.singularity.getAugmentationFactions(augName);
            if (allFactions.length === 0) {
                ns.tprint(`ERROR: [${ns.getScriptName()}]: no known sources for augmentation ${augName} despite being listed from ${factionName}!`);
                continue inner;
            }

            let otherSources: OtherSources;
            if (allFactions.length === 1) {
                if (allFactions[0] === factionName) {
                    const graftable = graftableAugs.includes(augName);
                    otherSources = { kind: "exclusive", graftable };
                } else {
                    ns.tprint(`ERROR: [${ns.getScriptName()}]: ${factionName} lists augmentation ${augName} but ${augName} is exclusive to ${allFactions[0]}`);
                    continue inner;
                }
            } else {
                const otherFactions = allFactions.filter(f => f !== factionName);
                const numberJoined = otherFactions.filter(f => factions.includes(f)).length;
                otherSources = { kind: 'factions', numberJoined, otherFactions };
            }
            augReqs.push({ augName, otherSources, reqRep })
        }
        table.push({ factionName, myRep, augReqs })
    }
    return table;
}

function display(ns: NS, table: FactionRepProgress[], flags: { [key: string]: string[] | ScriptArg }) {
    const verbose = flags.verbose ?? false;
    for (const entry of table) {
        const { factionName, augReqs, myRep } = entry;
        if (augReqs.length === 0) {
            ns.print(`SUCCESS: Faction ${entry.factionName} has no more purchasable augmentations.`);
            continue;
        } else {
            const sortedAugs = augReqs.toSorted((a, b) => a.reqRep - b.reqRep);
            const alreadyEnough = sortedAugs.filter((augReq) => augReq.reqRep <= entry.myRep);
            const needsMore = sortedAugs.filter((augReq) => augReq.reqRep > entry.myRep);
            if (needsMore.length === 0) {
                ns.print(`SUCCESS: ${factionName} is at enough reputation to purchase all remaining augmentations.`);
            } else {
                const closest = needsMore[0];
                const furthest = needsMore[needsMore.length - 1];

                const proximal = closest.reqRep - myRep;
                const distal = furthest.reqRep - myRep;
                if (verbose) {
                    ns.print(`WARN: ${factionName} needs more ${ns.formatNumber(distal)} reputation to purchase the remaining ${needsMore.length} out of ${sortedAugs.length} augmentations (${ns.formatNumber(proximal)} until next).`);
                } else {
                    ns.print(`WARN: ${factionName} needs more reputation to purchase remaining ${needsMore.length} augmentations.`);
                }
            }
        }
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags(Flags);
    return [];
}
