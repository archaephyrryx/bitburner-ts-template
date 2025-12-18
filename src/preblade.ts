import { NS, AutocompleteData } from '@ns';
import { GymStat, GymType, getTaskInfo } from "./global";

export const MIN_STR = 100;
export const MIN_AGI = 100;
export const MIN_DEF = 100;
export const MIN_DEX = 100;

export const PREFERRED_GYM = "Powerhouse Gym";
const SLEEP_SECONDS = 5;

export async function main(ns: NS) {
    const flags = ns.flags([["stealFocus", false]]);
    if (ns.bladeburner.inBladeburner()) {
        ns.toast("Already in bladeburner", "error", 2000);
        ns.exit();
    }

    const stealFocus = flags.stealFocus as boolean;

    const strTrained = await trainSTR(ns, MIN_STR, stealFocus);
    if (strTrained) {
        ns.toast(`Trained Strength to ${MIN_STR}`, "success", 2000);
    } else {
        ns.toast(`Strength Already >= ${MIN_STR}`, "info", 2000);
    }
    const defTrained = await trainDEF(ns, MIN_DEF, stealFocus);
    if (defTrained) {
        ns.toast(`Trained Defense to ${MIN_DEF}`, "success", 2000);
    } else {
        ns.toast(`Defense Already >= ${MIN_DEF}`, "info", 2000);
    }
    const dexTrained = await trainDEX(ns, MIN_DEX, stealFocus);
    if (dexTrained) {
        ns.toast(`Trained Dexterity to ${MIN_DEX}`, "success", 2000);
    } else {
        ns.toast(`Dexterity Already >= ${MIN_DEX}`, "info", 2000);
    }
    const agiTrained = await trainAGI(ns, MIN_AGI, stealFocus);
    if (agiTrained) {
        ns.toast(`Trained Agility to ${MIN_AGI}`, "success", 2000);
    } else {
        ns.toast(`Agility Already >= ${MIN_AGI}`, "info", 2000);
    }

    if (strTrained || defTrained || dexTrained || agiTrained) {
        ns.toast("Bladeburner training complete", "success", 2000);
    }

    if (ns.bladeburner.joinBladeburnerDivision()) {
        ns.toast("Joined Bladeburner Successfully", "success", 5000);
        ns.spawn("blade.js", { threads: 1, spawnDelay: 0 });
    } else {
        ns.toast("Could not join Bladeburner Division", "error", 5000);
        ns.exit();
    }
}


function workoutStat(ns: NS, stat: GymStat): boolean {
    if (ns.singularity.gymWorkout(PREFERRED_GYM, stat)) {
        return true;
    } else {
        ns.print(`Could not automatically train ${stat} at gym...`);
        return false;
    }
}

async function trainSTR(ns: NS, target: number, stealFocus = false): Promise<boolean> {
    return await trainAny(ns, "strength", target, stealFocus);
}

async function trainDEF(ns: NS, target: number, stealFocus = false): Promise<boolean> {
    return await trainAny(ns, "defense", target, stealFocus);
}

async function trainDEX(ns: NS, target: number, stealFocus = false): Promise<boolean> {
    return await trainAny(ns, "dexterity", target, stealFocus);
}

async function trainAGI(ns: NS, target: number, stealFocus = false): Promise<boolean> {
    return await trainAny(ns, "agility", target, stealFocus);
}

type GymSkill = keyof typeof GymType;

async function trainAny(ns: NS, stat: GymSkill, target: number, stealFocus = false): Promise<boolean> {
    let player = ns.getPlayer();
    if (player.skills[stat] >= target) { return false; }
    while (player.skills[stat] < target) {
        const taskInfo = getTaskInfo(ns);
        if (taskInfo.isGrafting) {
            ns.print("Grafting work in progress; will not interrupt until done...");
        } else if (taskInfo.gymStatsIncreased.includes(GymType[stat])) {
            if (stealFocus && !taskInfo.isFocused) {
                ns.singularity.setFocus(true);
            }
            ns.print("Maintaining current task...");
        } else {
            const tmp = workoutStat(ns, GymType[stat]);
            if (!tmp) { ns.exit(); }
        }
        await ns.sleep(SLEEP_SECONDS * 1000);
        player = ns.getPlayer();
    }
    return true;
}


export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["stealFocus", false]]);
    return [];
}
