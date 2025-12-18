import { NS, Fragment } from '@ns';

const SyncEvery = 10;

export async function main(ns: NS) {
    ns.ui.openTail();
    await runStanek(ns);
}

async function runStanek(ns: NS) {
    const defs = ns.stanek.fragmentDefinitions();
    const idMap = new Map(defs.map(frag => [frag.id, frag]));

    for (; ;) {
        const fragments = ns.stanek.activeFragments();
        ns.ui.openTail
        const ascCharge = fragments.toSorted((a, b) => a.numCharge - b.numCharge);

        for (let i = 0; i < ascCharge.length; i++) {
            const fragment = ascCharge[i];
            const { x, y, id } = fragment;
            const val = idMap.get(id);
            if (val == undefined || val.type == FragmentType.Booster) {
                continue;
            }
            await ns.stanek.chargeFragment(x, y);
        }
    }
}

export enum FragmentType {
    // Special fragments for the UI
    None,
    Delete,

    // Stats boosting fragments
    HackingChance,
    HackingSpeed,
    HackingMoney,
    HackingGrow,
    Hacking,
    Strength,
    Defense,
    Dexterity,
    Agility,
    Charisma,
    HacknetMoney,
    HacknetCost,
    Rep,
    WorkMoney,
    Crime,
    Bladeburner,

    // utility fragments.
    Booster,
}
