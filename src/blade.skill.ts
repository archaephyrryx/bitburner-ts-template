import { NS } from '@ns';

function rankBlade(ns: NS): boolean {
    const skillmods = new Map([
        ["Blade's Intuition", 2.0],
        ["Cloak", 0.8],
        ["Short-Circuit", 1.0],
        ["Digital Observer", 1.6],
        ["Tracer", 1.0],
        ["Overclock", 2.2],
        ["Reaper", 1.0],
        ["Evasive System", 1.0],
        ["Datamancer", 1.0],
        ["Cyber's Edge", 1.0],
        ["Hands of Midas", 0.3],
        ["Hyperdrive", 2.5],
    ]);
    const points = ns.bladeburner.getSkillPoints();
    const skills = ns.bladeburner.getSkillNames();

    const skillRatingCosts: [string, number, number][] = [];

    for (const skill of skills) {
        if (cannotUpgrade(ns, skill)) continue;
        const cost = ns.bladeburner.getSkillUpgradeCost(skill, 1);
        const weight = skillmods.get(skill);
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
        // ns.toast(`Purchased ${skill} level ${ns.bladeburner.getSkillLevel(skill)}`, 'success', 2000);
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

function cannotUpgrade(ns: NS, skill: string) {
    const LIMITS: Record<string, number> = { ["Overclock"]: 90 };
    const curLevel = ns.bladeburner.getSkillLevel(skill);
    return LIMITS[`${skill}`] !== undefined && curLevel >= LIMITS[`${skill}`];
}
