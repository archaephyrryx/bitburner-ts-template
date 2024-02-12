import { AutocompleteData, GangGenInfo, GangMemberInfo, NS } from "@ns";
import { canAfford } from "./money_helper";

export async function main(ns: NS) {
    const flags = ns.flags([["ascend", false]])
    if (!ns.gang.inGang()) {
        ns.tprint("ERROR: Gang Gang Kawaikunai-yo~!")
        ns.exit();
    }

    const gangInfo: GangGenInfo = ns.gang.getGangInformation();

    if (gangInfo.isHacking) {
        await hackingGang(ns, flags.ascend as boolean);
    } else {
        ns.tprint("Not configured to automate non-hacking gang.");
        ns.exit();
    }
}

async function hackingGang(ns: NS, autoAscend = false) {
    for (; ;) {
        const gangInfo = ns.gang.getGangInformation();
        const members = ns.gang.getMemberNames();

        for (const member of members) {
            const info = ns.gang.getMemberInformation(member);
            if (info.hack > 100) {
                if (gangInfo.wantedLevel < WANTED_LOW_TIDE) {
                    if (info.hack > 200) {
                        ns.gang.setMemberTask(member, "DDoS Attacks");
                    } else {
                        ns.gang.setMemberTask(member, "Phishing");
                    }
                } else if (gangInfo.wantedLevel > WANTED_HIGH_TIDE) {
                    ns.gang.setMemberTask(member, "Ethical Hacking")
                }
            } else {
                const res = ns.gang.getAscensionResult(member);
                if (res !== undefined && res.hack >= ASCENSION_RATIO && autoAscend)
                    ns.gang.ascendMember(member);
                ns.gang.setMemberTask(member, "Train Hacking");
            }
            for (const equip of ns.gang.getEquipmentNames()) {
                const stats = ns.gang.getEquipmentStats(equip);
                if (stats.hack !== undefined && stats.hack > 0) {
                    if (!info.upgrades.includes(equip)) {
                        ns.gang.purchaseEquipment(member, equip);
                    }
                }
            }
        }

        if (gangInfo.respect >= gangInfo.respectForNextRecruit) {
            ns.gang.recruitMember(`member-${members.length}`)
        }

        await ns.gang.nextUpdate();
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([["ascend", false]]);
    return [];
}

const WANTED_LOW_TIDE = 1.05;
const WANTED_HIGH_TIDE = 2;

const ASCENSION_RATIO = 1.25;
