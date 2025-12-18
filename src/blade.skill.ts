import { NS } from '@ns';
import { BladeburnerSkillName } from './global.enums';

const CustomWeightTable = new Map<BladeburnerSkillName, number>([
    [BladeburnerSkillName.BladesIntuition, 2.0],
    [BladeburnerSkillName.Cloak, 0.8],
    [BladeburnerSkillName.ShortCircuit, 1.0],
    [BladeburnerSkillName.DigitalObserver, 1.6],
    [BladeburnerSkillName.Tracer, 1.0],
    [BladeburnerSkillName.Overclock, 2.2],
    [BladeburnerSkillName.Reaper, 1.0],
    [BladeburnerSkillName.EvasiveSystem, 1.0],
    [BladeburnerSkillName.Datamancer, 1.0],
    [BladeburnerSkillName.CybersEdge, 1.0],
    [BladeburnerSkillName.HandsOfMidas, 0.3],
    [BladeburnerSkillName.Hyperdrive, 2.5],
]);

/**
 * Ranks up the most cost-effective Bladeburner skill available, based on custom rank-weights.
 * @param ns {NS} NetScript instancee
 * @returns {boolean} true if a skill was ranked up, false otherwise (including error)
 */
function rankBlade(ns: NS): boolean {
    const points = ns.bladeburner.getSkillPoints();
    const skills = ns.bladeburner.getSkillNames();

    const skillRatingCosts: [BladeburnerSkillName, number, number][] = [];

    for (const skill of skills) {
        if (cannotUpgrade(ns, skill)) continue;
        const cost = ns.bladeburner.getSkillUpgradeCost(skill, 1);
        const weight = CustomWeightTable.get(skill);
        if (weight == undefined) {
            ns.tprint(`ERROR: bad skill ${skill}`);
            return false;
        }
        const rating = cost / weight;
        skillRatingCosts.push([skill, rating, cost]);
    }
    skillRatingCosts.sort((a, b) => a[1] - b[1]);
    const [skill, , cost] = skillRatingCosts[0];
    if (cost <= points) {
        if (!ns.bladeburner.upgradeSkill(skill, 1)) {
            return false;
        }
        return true;
    }
    else {
        const curLvl = ns.bladeburner.getSkillLevel(skill);
        ns.print(`Next Skill: ${skill} [${curLvl} => ${curLvl + 1}] (${points}/${cost} required SP)`);
        return false;
    }
}

export async function main(ns: NS) {
    if (!ns.bladeburner.inBladeburner()) ns.exit();
    ns.disableLog('sleep')
    for (; ;) {
        while (rankBlade(ns)) {
            continue;
        }
        await ns.sleep(100);
        ns.clearLog();
    }
    return;
}

function cannotUpgrade(ns: NS, skill: BladeburnerSkillName) {
    const LIMITS: Record<string, number> = { ["Overclock"]: 90 };
    const curLevel = ns.bladeburner.getSkillLevel(skill);
    return LIMITS[`${skill}`] !== undefined && curLevel >= LIMITS[`${skill}`];
}
