import { NS } from "@ns";
import { joinFaction, travelTo } from "./faction";


export const Milestones: ((ns: NS) => Promise<void>)[] = [
    async (ns: NS) => {
        /* Obtain Neuroreceptor Management Implant via grafting or purchase */
        const augmentName = "Neuroreceptor Management Implant";
        const factionName = "Tian Di Hui";
        if (augmentName in ns.singularity.getOwnedAugmentations()) {
            return;
        }
        const price = ns.singularity.getAugmentationPrice(augmentName);
        const reqRep = ns.singularity.getAugmentationRepReq(augmentName);
        if (await joinFaction(ns, factionName)) {
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
