import { NS } from '@ns';
import { recordRandom } from './awdangit';

export async function main(ns: NS) {
    recordRandom(ns);
    if (ns.getPlayer().city !== "Aevum" && !ns.singularity.travelToCity("Aevum")) {
        ns.tprint("Failed to travel to Aevum!");
        ns.exit();
    }
    if (!ns.singularity.goToLocation("Iker Molina Casino")) {
        ns.tprint("Failed to travel to Iker Molina Casino!");
        ns.exit();
    }
    ns.toast("Let's go gambling!", "success", 3000);
    ns.ui.openTail();
    return;
}
