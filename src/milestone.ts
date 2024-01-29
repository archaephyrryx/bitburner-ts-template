import { NS } from "@ns";
import { joinFaction, travelTo } from "./faction";
import { BUDGET, makePurchase } from "./budget";


export const Milestones: ((ns: NS) => Promise<void>)[] = [
    async (ns: NS) => {
        /* Obtain Neuroreceptor Management Implant via grafting or purchase */
        const augmentName = "Neuroreceptor Management Implant";
        const factionName = "Tian Di Hui";
        if (augmentName in ns.singularity.getOwnedAugmentations()) {
            return;
        }
        if (await joinFaction(ns, factionName)) {
            const repReq = ns.singularity.getAugmentationRepReq(augmentName);
            for (; ;) {
                const rep = ns.singularity.getFactionRep(factionName);
                if (rep >= repReq) {
                    break;
                }
                await ns.sleep(1000);
            }
            const price = ns.singularity.getAugmentationPrice(augmentName);
            const id = await BUDGET.request(ns, price);
            await makePurchase(ns, id, (async (ns: NS) => ns.singularity.purchaseAugmentation(factionName, augmentName)));
            return;
        } else {
            await travelTo(ns, "New Tokyo");
            ns.grafting.graftAugmentation(augmentName);
        }
    },
];

export async function main(ns: NS) {
    await Milestones[0](ns);
}
