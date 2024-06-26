import { AutocompleteData, GangGenInfo, GangMemberAscension, GangMemberInfo, NS } from "@ns";
import { bold, reset } from "./helper";

const HackNames = ["Zer0", "M0nad", "Du0", "Trinity", "Tetra", "Quine", "Hex", "N4N0", "0ctal", "N0N4", "D3c1m4t0r", "L-V"];
const CombatNames = ["No-Body", "OnePunchMan", "DoubleTap", "Trice", "QuadBarrel", "Quincy", "SixShooter", "DeviantSeptum", "Octo-Vlad", "Nine-Iron", "10ne_wolf", "Ki11er"];

const CYCLES_PER_SECOND = 5;

export async function main(ns: NS) {
    const flags = ns.flags([["ascend", false], ["noEquip", false], ["focus", "random"]]);
    ns.disableLog("gang.purchaseEquipment");
    ns.disableLog("gang.setMemberTask");
    if (!ns.gang.inGang()) {
        ns.tprint("ERROR: Gang Gang Kawaikunai-yo~!")
        ns.exit();
    }

    const gangInfo: GangGenInfo = ns.gang.getGangInformation();

    if (gangInfo.isHacking) {
        await hackingGang(
            ns,
            flags.ascend as boolean,
            flags.noEquip as boolean,
            flags.autoAugment as boolean,
            flags.focus as Focus
        );
    } else {
        await combatGang(ns, flags.ascend as boolean, flags.noEquip as boolean, flags.autoAugment as boolean, flags.focus as Focus);
    }
}

async function hackingGang(
    ns: NS,
    autoAscend: boolean,
    noEquip: boolean,
    autoAugment: boolean,
    focus: Focus
) {
    ns.tail();
    const tasks = ns.gang.getTaskNames();

    for (; ;) {
        ns.clearLog();
        const gangInfo = ns.gang.getGangInformation();
        const members = ns.gang.getMemberNames();
        const absPenaltyPercent = Math.abs(
            (1 - ns.formulas.gang.wantedPenalty(gangInfo)) * 100
        );

        for (const member of members) {
            const info = ns.gang.getMemberInformation(member);
            const res = ns.gang.getAscensionResult(member);
            if (res !== undefined && (res.hack >= ASCENSION_RATIO)) {
                if (autoAscend && (info.upgrades.length === 0 || !noEquip) && (gangInfo.respect - info.earnedRespect >= 3 * gangInfo.wantedLevel)) {
                    ns.gang.ascendMember(member);
                    ns.toast(`Ascended ${member}, now setting to task "Train Hacking"`, "success", 3500);
                    ns.gang.setMemberTask(member, "Train Hacking");
                } else {
                    ns.print(`${bold}INFO: Ascension candidate ${member} will continue task ${info.task} until manually ascended...${reset}`);
                }
            } else if (absPenaltyPercent >= WANTED_PENALTY_HIGH_WATERMARK) {
                if (info.respectGain < info.wantedLevelGain || (absPenaltyPercent >= WANTED_PENALTY_LOW_WATERMARK && info.wantedLevelGain < 0)) {
                    ns.gang.setMemberTask(member, "Ethical Hacking");
                    ns.print(`INFO: Wanted Level too high, ${member} will perform "Ethical Hacking"`)
                } else {
                    assignHackingTask(ns, tasks, member, info, gangInfo, "respect");
                }
            } else if (res === undefined) {
                ns.print(`INFO: Training ${member}, as auto-ascension is either disabled, impossible, or not efficacious`);
                if (info.cha_exp * HACK_CHA_RATIO < info.hack_exp) {
                    ns.gang.setMemberTask(member, "Train Charisma");
                } else {
                    ns.gang.setMemberTask(member, "Train Hacking");
                }
            } else if (
                absPenaltyPercent < WANTED_PENALTY_HIGH_WATERMARK ||
                gangInfo.wantedLevel == 1
            ) {

                const myFocus: Focus =
                    (gangInfo.respectForNextRecruit < Number.MAX_SAFE_INTEGER || info.earnedRespect <= (gangInfo.respect / (members.length + 1))) ? "respect" :
                        focus === "random" ? ((gangInfo.territory < 0.99) ? pickRandom(["respect", "money", "power"]) : pickRandom(["respect", "money"]))
                            : focus;
                assignHackingTask(ns, tasks, member, info, gangInfo, myFocus);
            } else {
                ns.print(
                    `INFO: Wanted Level is acceptable, ${member} will continue performing "${info.task}"`
                );
            }
            for (const equip of ns.gang.getEquipmentNames()) {
                const stats = ns.gang.getEquipmentStats(equip);
                if (
                    (stats.hack !== undefined && stats.hack > 0) ||
                    (stats.cha !== undefined && stats.cha > 0) ||
                    focus == "power" ||
                    focus == "random"
                ) {
                    if (!info.upgrades.includes(equip)) {
                        if (
                            !noEquip ||
                            (ns.gang.getEquipmentType(equip) === "Augmentation" &&
                                autoAugment)
                        ) {
                            ns.gang.purchaseEquipment(member, equip);
                        }
                    }
                }
            }
        }

        if (gangInfo.respect >= gangInfo.respectForNextRecruit) {
            const name =
                members.length > HackNames.length
                    ? `member-${members.length}`
                    : HackNames[members.length];
            if (ns.gang.recruitMember(name)) {
                ns.gang.setMemberTask(name, "Train Hacking");
            } else {
                for (const name of HackNames) {
                    if (members.includes(name)) {
                        continue;
                    }
                    if (ns.gang.recruitMember(name)) {
                        ns.gang.setMemberTask(name, "Train Hacking");
                    }
                }
            }
        }

        ns.print(`${bold}Respect: ${ns.formatNumber(gangInfo.respect)} (${ns.formatNumber(gangInfo.respectGainRate * CYCLES_PER_SECOND)}/s)\tEarning $${ns.formatNumber(gangInfo.moneyGainRate * CYCLES_PER_SECOND)}/s${reset}`);
        await ns.gang.nextUpdate();
    }
}

type Focus = 'money' | 'respect' | 'power' | 'random';

function assignHackingTask(ns: NS, tasks: string[], memberName: string, memberInfo: GangMemberInfo, gangInfo: GangGenInfo, focus: Focus = 'money') {
    let bestTask: string | undefined = undefined;
    let bestTaskRespGain = -1;
    let bestTaskMoneyGain = -1;

    if (focus == 'power') {
        if (memberInfo.agi < 50 || memberInfo.def < 50 || memberInfo.str < 50 || memberInfo.dex < 50) {
            bestTask = "Train Combat";
        } else {
            bestTask = "Territory Warfare";
        }
    } else {
        for (const task of tasks) {
            const stats = ns.gang.getTaskStats(task);
            const respGain = ns.formulas.gang.respectGain(gangInfo, memberInfo, stats);
            const moneyGain = ns.formulas.gang.moneyGain(gangInfo, memberInfo, stats);
            if ((respGain > bestTaskRespGain && focus === 'respect') || (moneyGain > bestTaskMoneyGain && focus === 'money')) {
                bestTask = task;
                bestTaskRespGain = respGain;
                bestTaskMoneyGain = moneyGain;
            }
        }
    }

    const fallbackTask = "Train Hacking";

    if (bestTask !== undefined && bestTask !== "Unassigned" && ns.gang.setMemberTask(memberName, bestTask)) {
        ns.print(`SUCCESS: Gang member ${memberName} now performing task "${bestTask}"`)
        return;
    } else {
        ns.print(`WARNING: Gang member ${memberName} falling back on task "${fallbackTask}"`)
        ns.gang.setMemberTask(memberName, fallbackTask);
        return;
    }
}


// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["ascend", false], ["noEquip", false], ["focus", "random"]]);
    return [];
}

function pickRandom(opts: Focus[]): Focus {
    return opts[Math.floor(Math.random() * opts.length)]
}

async function combatGang(
    ns: NS,
    autoAscend: boolean,
    noEquip: boolean,
    autoAugment: boolean,
    focus: Focus
) {
    ns.tail();
    const tasks = ns.gang.getTaskNames();

    for (; ;) {
        ns.clearLog();
        const gangInfo = ns.gang.getGangInformation();
        const members = ns.gang.getMemberNames();
        const absPenaltyPercent = Math.abs(
            (1 - ns.formulas.gang.wantedPenalty(gangInfo)) * 100
        );

        for (const member of members) {
            const info = ns.gang.getMemberInformation(member);
            const res = ns.gang.getAscensionResult(member);
            if (res !== undefined && shouldAscendCombat(res)) {
                if (autoAscend && (info.upgrades.length === 0 || !noEquip) && (gangInfo.respect - info.earnedRespect >= 3 * gangInfo.wantedLevel)) {
                    ns.gang.ascendMember(member);
                    ns.toast(`Ascended ${member}, now setting to task "Train Combat"`, "success", 3500);
                    ns.gang.setMemberTask(member, "Train Combat");
                } else {
                    ns.print(`${bold}INFO: Ascension candidate ${member} will continue task ${info.task} until manually ascended...${reset}`);
                }
            } else if (absPenaltyPercent >= WANTED_PENALTY_HIGH_WATERMARK) {
                if (gangInfo.wantedLevel > 1 && (info.respectGain < info.wantedLevelGain || (absPenaltyPercent >= WANTED_PENALTY_LOW_WATERMARK && info.wantedLevelGain < 0))) {
                    ns.gang.setMemberTask(member, "Vigilante Justice");
                    ns.print(`INFO: Wanted Level too high, ${member} will perform "Vigilante Justice"`)
                } else {
                    assignCombatTask(ns, tasks, member, info, gangInfo, "respect");
                }
            } else if (res === undefined) {
                ns.print(`INFO: Training ${member}, as auto-ascension is either disabled, impossible, or not efficacious`);
                if (info.cha_exp * STR_CHA_RATIO < info.str_exp) {
                    ns.gang.setMemberTask(member, "Train Charisma");
                } else {
                    ns.gang.setMemberTask(member, "Train Combat");
                }
            } else if (
                absPenaltyPercent < WANTED_PENALTY_HIGH_WATERMARK ||
                gangInfo.wantedLevel == 1
            ) {

                const myFocus: Focus =
                    (gangInfo.respectForNextRecruit < Number.MAX_SAFE_INTEGER || info.earnedRespect <= gangInfo.wantedLevel) ? "respect" :
                        focus === "random" ? ((gangInfo.territory < 0.99) ? pickRandom(["respect", "money", "power"]) : pickRandom(["respect", "money"]))
                            : focus;
                assignCombatTask(ns, tasks, member, info, gangInfo, myFocus);
            } else {
                ns.print(
                    `INFO: Wanted Level is acceptable, ${member} will continue performing "${info.task}"`
                );
            }
            for (const equip of ns.gang.getEquipmentNames()) {
                const stats = ns.gang.getEquipmentStats(equip);
                if (
                    ((stats.hack ?? 0) > 0) ||
                    ((stats.str ?? 0) > 0) ||
                    ((stats.def ?? 0) > 0) ||
                    ((stats.dex ?? 0) > 0) ||
                    ((stats.agi ?? 0) > 0) ||
                    ((stats.cha ?? 0) > 0)
                ) {
                    if (!info.upgrades.includes(equip)) {
                        if (
                            !noEquip ||
                            (ns.gang.getEquipmentType(equip) === "Augmentation" &&
                                autoAugment)
                        ) {
                            ns.gang.purchaseEquipment(member, equip);
                        }
                    }
                }
            }
        }

        if (gangInfo.respect >= gangInfo.respectForNextRecruit) {
            const name =
                members.length > CombatNames.length
                    ? `member-${members.length}`
                    : CombatNames[members.length];
            if (ns.gang.recruitMember(name)) {
                ns.gang.setMemberTask(name, "Train Combat");
            } else {
                for (const name of CombatNames) {
                    if (members.includes(name)) {
                        continue;
                    }
                    if (ns.gang.recruitMember(name)) {
                        ns.gang.setMemberTask(name, "Train Combat");
                    }
                }
            }
        }

        ns.print(`${bold}Respect: ${ns.formatNumber(gangInfo.respect)} (${ns.formatNumber(gangInfo.respectGainRate * CYCLES_PER_SECOND)}/s)\tEarning $${ns.formatNumber(gangInfo.moneyGainRate * CYCLES_PER_SECOND)}/s${reset}`);
        await ns.gang.nextUpdate();
    }
}



function assignCombatTask(ns: NS, tasks: string[], memberName: string, memberInfo: GangMemberInfo, gangInfo: GangGenInfo, focus: Focus = 'money') {
    let bestTask: string | undefined = undefined;
    let bestTaskRespGain = -1;
    let bestTaskMoneyGain = -1;

    if (focus == 'power') {
        if (memberInfo.agi < TW_MIN_STAT || memberInfo.def < TW_MIN_STAT || memberInfo.str < TW_MIN_STAT || memberInfo.dex < TW_MIN_STAT) {
            bestTask = "Train Combat";
        } else {
            bestTask = "Territory Warfare";
        }
    } else {
        for (const task of tasks) {
            const stats = ns.gang.getTaskStats(task);
            const respGain = ns.formulas.gang.respectGain(gangInfo, memberInfo, stats);
            const moneyGain = ns.formulas.gang.moneyGain(gangInfo, memberInfo, stats);
            if ((respGain > bestTaskRespGain && focus === 'respect') || (moneyGain > bestTaskMoneyGain && focus === 'money')) {
                bestTask = task;
                bestTaskRespGain = respGain;
                bestTaskMoneyGain = moneyGain;
            }
        }
    }

    const fallbackTask = "Train Combat";

    if (bestTask !== undefined && bestTask !== "Unassigned" && ns.gang.setMemberTask(memberName, bestTask)) {
        ns.print(`SUCCESS: Gang member ${memberName} now performing task "${bestTask}"`)
        return;
    } else {
        ns.print(`WARNING: Gang member ${memberName} falling back on task "${fallbackTask}"`)
        ns.gang.setMemberTask(memberName, fallbackTask);
        return;
    }
}

const TW_MIN_STAT = 100;
// const MIN_ASC_MULT = 1.00025;

const WANTED_PENALTY_HIGH_WATERMARK = 25.00;
const WANTED_PENALTY_LOW_WATERMARK = 5.00;

const ASCENSION_RATIO = 1.5;
const HACK_CHA_RATIO = 3;
const STR_CHA_RATIO = 10;

function shouldAscendCombat(res: GangMemberAscension) {
    return (res.str >= ASCENSION_RATIO || res.dex >= ASCENSION_RATIO || res.def >= ASCENSION_RATIO || res.agi >= ASCENSION_RATIO)
    // && res.str >= MIN_ASC_MULT
    // && res.dex >= MIN_ASC_MULT
    // && res.def >= MIN_ASC_MULT
    // && res.agi >= MIN_ASC_MULT
}
