import { AutocompleteData, GangGenInfo, GangMemberInfo, NS } from "@ns";
import { bold, reset } from "./helper";

const Names = ["Zer0", "M0nad", "Du0", "Trinity", "Tetra", "Quine", "Hex", "N4N0", "0ctal", "N0N4", "D3c1m4t0r", "L-V", "B4k3r"];

export async function main(ns: NS) {
    const flags = ns.flags([["ascend", false], ["equip", false], ["focus", "random"], ["autoAugment", false]]);
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
            flags.equip as boolean,
            flags.autoAugment as boolean,
            flags.focus as Focus
        );
    } else {
        ns.tprint("Not configured to automate non-hacking gang.");
        ns.exit();
    }
}

async function hackingGang(
    ns: NS,
    autoAscend: boolean,
    autoEquip: boolean,
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
            if (res !== undefined && (res.hack >= ASCENSION_RATIO && res.cha >= ASCENSION_RATIO)) {
                if (autoAscend && (info.upgrades.length === 0 || autoEquip)) {
                    ns.gang.ascendMember(member);
                    ns.print(
                        `SUCCESS: Ascended ${member}, now setting to task "Train Hacking"`
                    );
                    ns.gang.setMemberTask(member, "Train Hacking");
                    continue;
                } else {
                    ns.print(`${bold}INFO: Ascension candidate ${member} will continue task ${info.task} until manually ascended...${reset}`);
                    continue;
                }
            }
            if (absPenaltyPercent >= WANTED_PENALTY_HIGH_WATERMARK) {
                if (gangInfo.respectGainRate < gangInfo.wantedLevelGainRate) {
                    assignHackingTask(ns, tasks, member, info, gangInfo, "respect");
                } else {
                    ns.gang.setMemberTask(member, "Ethical Hacking");
                }
            } else if (res === undefined) {
                ns.print(
                    `INFO: Training ${member}, as auto-ascension is either disabled, impossible, or not efficacious`
                );
                if (info.cha_exp * HACK_CHA_RATIO < info.hack_exp) {
                    ns.gang.setMemberTask(member, "Train Charisma");
                } else {
                    ns.gang.setMemberTask(member, "Train Hacking");
                }
                continue;
            } else if (
                gangInfo.wantedLevel > 1 &&
                ns.gang.getMemberInformation(member).task == "Ethical Hacking"
            ) {
                ns.print(
                    `INFO: Wanted Level Penalty above minimum, ${member} will continue performing task "Ethical Hacking"`
                );
                continue;
            } else if (
                absPenaltyPercent < WANTED_PENALTY_HIGH_WATERMARK ||
                gangInfo.wantedLevel == 1
            ) {
                const myFocus: Focus =
                    focus === "random"
                        ? (["respect", "money", "power"] as Focus[])[
                        Math.floor(Math.random() * 3)
                        ]
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
                            autoEquip ||
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
                members.length > Names.length
                    ? `member-${members.length}`
                    : Names[members.length];
            if (ns.gang.recruitMember(name)) {
                ns.gang.setMemberTask(name, "Train Hacking");
            } else {
                for (const name of Names) {
                    if (members.includes(name)) {
                        continue;
                    }
                    if (ns.gang.recruitMember(name)) {
                        ns.gang.setMemberTask(name, "Train Hacking");
                    }
                }
            }
        }

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

    if (bestTask !== undefined && ns.gang.setMemberTask(memberName, bestTask)) {
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
    data.flags([["ascend", false], ["equip", false], ["focus", "random"]]);
    return [];
}

const WANTED_PENALTY_HIGH_WATERMARK = 10;

const ASCENSION_RATIO = 1.25;
const HACK_CHA_RATIO = 3;
