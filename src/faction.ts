import { CityName, NS, PlayerRequirement, Skills } from '@ns';
import { BUDGET, makePurchase } from './budget';

/**
 * Attempts to fulfill as many requirements as possible to join a faction, or aborts if
 * the process for joining is not simple enough to automate without other helper scripts.
 *
 * Returns whether the faction was successfully joined, or it already in said faction.
 */
export async function joinFaction(ns: NS, factionName: string): Promise<boolean> {
    if (factionName in ns.getPlayer().factions) {
        return true;
    }
    const invites = ns.singularity.checkFactionInvitations();
    if (factionName in invites) {
        ns.singularity.joinFaction(factionName);
        return true;
    }
    const reqs: PlayerRequirement[] = ns.singularity.getFactionInviteRequirements(factionName);

    const tasks: Genie[] = [];

    for (const req of reqs) {
        switch (req.type) {
            case "money":
                tasks.push(moneyWish(req.money));
                break;
            case "skills":
                for (const skill in req.skills) {
                    tasks.push(skillWish(skill as keyof Skills, req.skills[skill as keyof Skills] ?? 0))
                }
                break;
            case "city":
                tasks.push(cityWish(req.city));
                break;
            default:
                // eslint-disable-next-line no-case-declarations
                const msg = `No logic for faction '${factionName}' join-condition: ${JSON.stringify(req)}`;
                ns.toast(msg, "error", 3000);
                ns.tprint(`ERROR: ${msg}`);
                return false;
        }
    }
    for (const genie of tasks) {
        if (!await genie(ns)) {
            return false;
        }
    }
    return true;
}

function free(): Genie {
    return (async (ns: NS) => true);
}

function moneyWish(amount: number): Genie {
    return (async (ns: NS) => {
        while (ns.getServerMoneyAvailable("home") < amount) {
            await ns.sleep(100);
        }
        return true;
    });
}

function skillWish(skill: keyof Skills, level: number): Genie {
    return (async (ns: NS) => {
        while (ns.getPlayer().skills[skill] < level) {
            await ns.sleep(100);
        }
        return true;
    });
}


const TravelCost = 200000;

function cityWish(city: CityName | string): Genie {
    return (async (ns: NS) => {
        const id = await BUDGET.request(ns, TravelCost);
        const ret = await makePurchase(ns, id, (async (ns: NS) => {
            return ns.singularity.travelToCity(city as CityName | `${CityName}`);
        }));
        return ret;
    });
}

export async function travelTo(ns: NS, city: CityName | string): Promise<boolean> {
    const ret = await cityWish(city)(ns);
    return ret;
}



type Genie = (ns: NS) => Promise<boolean>;
