import { AutocompleteData, GangGenInfo, GangMemberInfo, NS } from "@ns";
import { bold, reset } from "./helper";

const Names = ["Zer0", "M0nad", "Du0", "Trinity", "Tetra", "Quine", "Hex", "N4N0", "0ctal", "N0N4", "D3c1m4t0r", "L-V", "B4k3r"];

const CYCLES_PER_SECOND = 5;

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
            if (res !== undefined && (res.hack >= ASCENSION_RATIO)) {
                if (autoAscend && (info.upgrades.length === 0 || autoEquip) && (gangInfo.respect - info.earnedRespect >= 3 * gangInfo.wantedLevel)) {
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
    data.flags([["ascend", false], ["equip", false], ["focus", "random"]]);
    return [];
}

const WANTED_PENALTY_HIGH_WATERMARK = 25.00;
const WANTED_PENALTY_LOW_WATERMARK = 5.00;

const ASCENSION_RATIO = 1.25;
const HACK_CHA_RATIO = 3;

function pickRandom(opts: Focus[]): Focus {
    return opts[Math.floor(Math.random() * opts.length)]
}
