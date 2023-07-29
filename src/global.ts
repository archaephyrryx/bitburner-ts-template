export type NodeInfo = { name: string; skill: number; ports: number };
export const nodes: NodeInfo[] = [
    {
        name: "foodnstuff",
        skill: 1,
        ports: 0,
    },
    { name: "n00dles", skill: 1, ports: 0 },
    {
        name: "sigma-cosmetics",
        skill: 5,
        ports: 0,
    },
    { name: "joesguns", skill: 10, ports: 0 },
    {
        name: "nectar-net",
        skill: 20,
        ports: 0,
    },
    {
        name: "hong-fang-tea",
        skill: 30,
        ports: 0,
    },
    {
        name: "harakiri-sushi",
        skill: 40,
        ports: 0,
    },
    { name: "neo-net", skill: 50, ports: 1 },
    {
        name: "CSEC",
        skill: 53,
        ports: 1,
    },
    { name: "zer0", skill: 75, ports: 1 },
    {
        name: "max-hardware",
        skill: 80,
        ports: 1,
    },
    {
        name: "iron-gym",
        skill: 100,
        ports: 1,
    },
    { name: "phantasy", skill: 100, ports: 2 },
    {
        name: "silver-helix",
        skill: 150,
        ports: 2,
    },
    {
        name: "omega-net",
        skill: 190,
        ports: 2,
    },
    { name: "avmnite-02h", skill: 202, ports: 2 },
    {
        name: "crush-fitness",
        skill: 251,
        ports: 2,
    },
    {
        name: "johnson-ortho",
        skill: 288,
        ports: 2,
    },
    { name: "the-hub", skill: 308, ports: 2 },
    {
        name: "computek",
        skill: 301,
        ports: 3,
    },
    {
        name: "I.I.I.I",
        skill: 341,
        ports: 3,
    },
    { name: "rothman-uni", skill: 373, ports: 3 },
    {
        name: "netlink",
        skill: 397,
        ports: 3,
    },
    {
        name: "catalyst",
        skill: 410,
        ports: 3,
    },
    { name: "summit-uni", skill: 467, ports: 3 },
    {
        name: "millenium-fitness",
        skill: 503,
        ports: 3,
    },
    {
        name: "rho-construction",
        skill: 520,
        ports: 3,
    },
    {
        name: "aevum-police",
        skill: 422,
        ports: 4,
    },
    { name: "run4theh111z", skill: 528, ports: 4 },
    {
        name: ".",
        skill: 537,
        ports: 4,
    },
    { name: "alpha-ent", skill: 558, ports: 4 },
    {
        name: "syscore",
        skill: 562,
        ports: 4,
    },
    {
        name: "lexo-corp",
        skill: 700,
        ports: 4,
    },
    { name: "snap-fitness", skill: 740, ports: 4 },
    {
        name: "global-pharm",
        skill: 769,
        ports: 4,
    },
    {
        name: "unitalife",
        skill: 792,
        ports: 4,
    },
    { name: "zb-def", skill: 796, ports: 4 },
    {
        name: "univ-energy",
        skill: 807,
        ports: 4,
    },
    {
        name: "applied-energetics",
        skill: 816,
        ports: 4,
    },
    { name: "nova-med", skill: 834, ports: 4 },
    {
        name: "darkweb",
        skill: 1,
        ports: 5,
    },
    {
        name: "zb-institute",
        skill: 755,
        ports: 5,
    },
    { name: "microdyne", skill: 814, ports: 5 },
    {
        name: "zeus-med",
        skill: 831,
        ports: 5,
    },
    {
        name: "titan-labs",
        skill: 844,
        ports: 5,
    },
    { name: "solaris", skill: 845, ports: 5 },
    {
        name: "aerocorp",
        skill: 857,
        ports: 5,
    },
    {
        name: "galactic-cyber",
        skill: 858,
        ports: 5,
    },
    { name: "deltaone", skill: 859, ports: 5 },
    {
        name: "vitalife",
        skill: 863,
        ports: 5,
    },
    {
        name: "taiyang-digital",
        skill: 881,
        ports: 5,
    },
    { name: "icarus", skill: 897, ports: 5 },
    {
        name: "helios",
        skill: 900,
        ports: 5,
    },
    {
        name: "omnia",
        skill: 901,
        ports: 5,
    },
    { name: "infocomm", skill: 912, ports: 5 },
    {
        name: "The-Cave",
        skill: 925,
        ports: 5,
    },
    { name: "stormtech", skill: 947, ports: 5 },
    {
        name: "defcomm",
        skill: 984,
        ports: 5,
    },
    {
        name: "b-and-a",
        skill: 1011,
        ports: 5,
    },
    { name: "clarkinc", skill: 1019, ports: 5 },
    {
        name: "blade",
        skill: 1025,
        ports: 5,
    },
    {
        name: "powerhouse-fitness",
        skill: 1029,
        ports: 5,
    },
    { name: "4sigma", skill: 1051, ports: 5 },
    {
        name: "omnitek",
        skill: 1051,
        ports: 5,
    },
    {
        name: "kuai-gong",
        skill: 1186,
        ports: 5,
    },
    { name: "nwo", skill: 1212, ports: 5 },
    {
        name: "fulcrumtech",
        skill: 1217,
        ports: 5,
    },
    {
        name: "fulcrumassets",
        skill: 1255,
        ports: 5,
    },
    { name: "megacorp", skill: 1296, ports: 5 },
    {
        name: "ecorp",
        skill: 1345,
        ports: 5,
    },
];

export const enum ServicePort {
    ControlSignal = 1,
    DiagnosticInfo = 2,
    MarketAction = 3,
    HackTargetServer = 4,
}
