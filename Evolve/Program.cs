using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Evolve
{
    enum Reg : int
    {
        R0 = 0, R1 = 1, R2 = 2, R3 = 3,
        R4 = 4, R5 = 5, R6 = 6, R7 = 7,
        R8 = 8, R9 = 9, RA = 10, RB = 11,
        RC = 12, RD = 13, RE = 14, RF = 15
    }

    interface Instruction
    {
        void Execute(Program.State state);
    }

    class NOP : Instruction
    {
        public void Execute(Program.State state)
        {
            bool finished = false;
            TryExecute(state, ref finished);
            if (!finished)
                state.counter++;
        }

        protected virtual void TryExecute(Program.State state, ref bool finished)
        {
        }

        protected virtual string Registers()
        {
            return "";
        }

        public override string ToString()
        {
            return this.GetType().Name + Registers();
        }
    }
    
    class Branch : NOP
    {
        private UInt16 _addr;
        public Branch(UInt16 addr)
        {
            _addr = addr;
        }

        protected virtual bool Condition(Program.State state)
        {
            return true;
        }

        protected sealed override void TryExecute(Program.State state, ref bool finished)
        {
            if (Condition(state))
            {
                state.counter = _addr;
                finished = true;
            }
        }

        protected override string Registers()
        {
            return " 0x" + _addr.ToString("X4");
        }
    }

    abstract class BranchCompare : Branch
    {
        protected Reg _rx, _ry;
        public BranchCompare(Reg rx, Reg ry, UInt16 addr)
            : base(addr)
        {
            _rx = rx;
            _ry = ry;
        }

        protected abstract bool Compare(Program.State state);

        protected sealed override bool Condition(Program.State state)
        {
            return Compare(state);
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " " + _ry.ToString() + base.Registers();
        }
    }

    sealed class BEQ : BranchCompare
    {
        public BEQ(Reg rx, Reg ry, UInt16 addr) : base(rx, ry, addr) {}

        protected override bool Compare(Program.State state)
        {
            return state.registers[(int)_rx] == state.registers[(int)_ry];
        }
    }

    sealed class BLT : BranchCompare
    {
        public BLT(Reg rx, Reg ry, UInt16 addr) : base(rx, ry, addr) { }

        protected override bool Compare(Program.State state)
        {
            return state.registers[(int)_rx] < state.registers[(int)_ry];
        }
    }

    abstract class BinaryOperation : NOP
    {
        protected Reg _rx, _ry, _rz;
        public BinaryOperation(Reg rx, Reg ry, Reg rz)
        {
            _rx = rx;
            _ry = ry;
            _rz = rz;
        }

        protected abstract UInt16 Operation(UInt16 v1, UInt16 v2);

        protected sealed override void TryExecute(Program.State state, ref bool finished)
        {
            state.registers[(int)_rz] = Operation(state.registers[(int)_rx], state.registers[(int)_ry]);
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " " + _ry.ToString() + " " + _rz.ToString();
        }
    }

    sealed class ADD : BinaryOperation
    {
        public ADD(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 + v2);
        }
    }

    sealed class SUB : BinaryOperation
    {
        public SUB(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 - v2);
        }
    }

    sealed class MLT : BinaryOperation
    {
        public MLT(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 * v2);
        }
    }

    sealed class MOD : BinaryOperation
    {
        public MOD(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            if (v2 == 0)
                return 0;
            return (UInt16)(v1 % v2);
        }
    }

    sealed class AND : BinaryOperation
    {
        public AND(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 & v2);
        }
    }

    sealed class LOR : BinaryOperation
    {
        public LOR(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 | v2);
        }
    }

    sealed class XOR : BinaryOperation
    {
        public XOR(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 ^ v2);
        }
    }

    sealed class SHR : BinaryOperation
    {
        public SHR(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 >> v2);
        }
    }

    sealed class SHL : BinaryOperation
    {
        public SHL(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return (UInt16)(v1 << v2);
        }
    }

    sealed class NEG : NOP
    {
        private Reg _rx, _ry;
        public NEG(Reg rx, Reg ry)
        {
            _rx = rx;
            _ry = ry;
        }

        protected override void TryExecute(Program.State state, ref bool finished)
        {
            state.registers[(int)_ry] = (UInt16)(~state.registers[(int)_rx]);
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " " + _ry.ToString();
        }
    }

    sealed class CPY : NOP
    {
        private Reg _rx, _ry;
        public CPY(Reg rx, Reg ry)
        {
            _rx = rx;
            _ry = ry;
        }

        protected override void TryExecute(Program.State state, ref bool finished)
        {
            state.registers[(int)_ry] = state.registers[(int)_rx];
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " " + _ry.ToString();
        }
    }

    sealed class SET : NOP
    {
        private Reg _rx;
        private UInt16 _value;
        public SET(Reg rx, UInt16 value)
        {
            _rx = rx;
            _value = value;
        }

        protected override void TryExecute(Program.State state, ref bool finished)
        {
            state.registers[(int)_rx] = _value;
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " 0x" + _value.ToString("X4");
        }
    }

    sealed class MRD : NOP
    {
        private Reg _rx, _ry;
        public MRD(Reg rx, Reg ry)
        {
            _rx = rx;
            _ry = ry;
        }

        protected override void TryExecute(Program.State state, ref bool finished)
        {
            state.registers[(int)_ry] = state.memory[(int)state.registers[(int)_rx]];
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " " + _ry.ToString();
        }
    }

    sealed class MWT : NOP
    {
        private Reg _rx, _ry;
        public MWT(Reg rx, Reg ry)
        {
            _rx = rx;
            _ry = ry;
        }

        protected override void TryExecute(Program.State state, ref bool finished)
        {
            state.memory[(int)state.registers[(int)_rx]] = state.registers[(int)_ry];
        }

        protected override string Registers()
        {
            return " " + _rx.ToString() + " " + _ry.ToString();
        }
    }

    class Program
    {
        private static readonly Dictionary<string, UInt32> InstructionSet;
        private static readonly Dictionary<UInt32, string> InstructionLookup;
        private static readonly Dictionary<string, Reg> RegisterSet;
        private const UInt32 InstructionMask = 0xff000000;
        static Program()
        {
            InstructionSet = new Dictionary<string, UInt32>();
            InstructionSet["NOP"] = 0x00000000;
            InstructionSet["BEQ"] = 0x10000000;
            InstructionSet["BLT"] = 0x11000000;
            InstructionSet["AND"] = 0x12000000;
            InstructionSet["LOR"] = 0x13000000;
            InstructionSet["XOR"] = 0x14000000;
            InstructionSet["SHR"] = 0x15000000;
            InstructionSet["SHL"] = 0x16000000;
            InstructionSet["NEG"] = 0x17000000;
            InstructionSet["ADD"] = 0x18000000;
            InstructionSet["SUB"] = 0x19000000;
            InstructionSet["MLT"] = 0x1a000000;
            InstructionSet["MOD"] = 0x1b000000;
            InstructionSet["CPY"] = 0x1c000000;
            InstructionSet["SET"] = 0x1d000000;
            InstructionSet["MRD"] = 0x1e000000;
            InstructionSet["MWT"] = 0x1f000000;

            InstructionLookup = new Dictionary<UInt32, string>();
            foreach (string key in InstructionSet.Keys)
            {
                UInt32 val = InstructionSet[key];
                InstructionLookup[val] = key;
            }

            RegisterSet = new Dictionary<string, Reg>();
            RegisterSet["R0"] = Reg.R0;
            RegisterSet["R1"] = Reg.R1;
            RegisterSet["R2"] = Reg.R2;
            RegisterSet["R3"] = Reg.R3;
            RegisterSet["R4"] = Reg.R4;
            RegisterSet["R5"] = Reg.R5;
            RegisterSet["R6"] = Reg.R6;
            RegisterSet["R7"] = Reg.R7;
            RegisterSet["R8"] = Reg.R8;
            RegisterSet["R9"] = Reg.R9;
            RegisterSet["RA"] = Reg.RA;
            RegisterSet["RB"] = Reg.RB;
            RegisterSet["RC"] = Reg.RC;
            RegisterSet["RD"] = Reg.RD;
            RegisterSet["RE"] = Reg.RE;
            RegisterSet["RF"] = Reg.RF;
        }

        private static UInt32 CodifyLine(string line)
        {
            string[] seg = line.ToUpper().Split(' ');
            if (!InstructionSet.ContainsKey(seg[0]))
                return InstructionSet["NOP"];

            UInt32 instruction = InstructionSet[seg[0]];
            switch (seg[0])
            {
                case "BEQ":
                case "BLT":
                    instruction |= (UInt32)((int)RegisterSet[seg[1]] << 20);
                    instruction |= (UInt32)((int)RegisterSet[seg[2]] << 16);
                    instruction |= UInt32.Parse(seg[3]);
                    break;
                case "AND":
                case "LOR":
                case "XOR":
                case "SHR":
                case "SHL":
                case "ADD":
                case "SUB":
                case "MLT":
                case "MOD":
                    instruction |= (UInt32)((int)RegisterSet[seg[1]] << 16);
                    instruction |= (UInt32)((int)RegisterSet[seg[2]] << 8);
                    instruction |= (UInt32)RegisterSet[seg[3]];
                    break;
                case "NEG":
                case "CPY":
                case "MRD":
                case "MWT":
                    instruction |= (UInt32)((int)RegisterSet[seg[1]] << 8);
                    instruction |= (UInt32)RegisterSet[seg[2]];
                    break;
                case "SET":
                    instruction |= (UInt32)((int)RegisterSet[seg[1]] << 16);
                    instruction |= UInt32.Parse(seg[2]);
                    break;
                default:
                    break;
            }

            return instruction;
        }
        
        public static UInt32[] Codify(string content)
        {
            UInt32[] prog = new UInt32[(int)UInt16.MaxValue];
            for (int i = 0; i < prog.Length; i++)
                prog[i] = InstructionSet["NOP"];

            string[] lines = content.Split(new string[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < lines.Length; i++)
            {
                prog[i] = CodifyLine(lines[i]);
            }

            return prog;
        }

        private static Instruction Parse(UInt32 code)
        {
            UInt16 value = (UInt16)(code & 0xffff);
            Reg rp = (Reg)((code & 0x00f00000) >> 20);
            Reg rq = (Reg)((code & 0x000f0000) >> 16);
            Reg rr = (Reg)((code & 0x00000f00) >> 8);
            Reg rs = (Reg)(code & 0x0000000f);
            if (!InstructionLookup.ContainsKey(InstructionMask & code))
                return new NOP();

            switch (InstructionLookup[InstructionMask & code])
            {
                case "BEQ":
                    return new BEQ(rp, rq, value);
                case "BLT":
                    return new BLT(rp, rq, value);
                case "AND":
                    return new AND(rq, rr, rs);
                case "LOR":
                    return new LOR(rq, rr, rs);
                case "XOR":
                    return new XOR(rq, rr, rs);
                case "SHR":
                    return new SHR(rq, rr, rs);
                case "SHL":
                    return new SHL(rq, rr, rs);
                case "ADD":
                    return new ADD(rq, rr, rs);
                case "SUB":
                    return new SUB(rq, rr, rs);
                case "MLT":
                    return new MLT(rq, rr, rs);
                case "MOD":
                    return new MOD(rq, rr, rs);
                case "NEG":
                    return new NEG(rr, rs);
                case "CPY":
                    return new CPY(rr, rs);
                case "MRD":
                    return new MRD(rr, rs);
                case "MWT":
                    return new MWT(rr, rs);
                case "SET":
                    return new SET(rq, value);
                default:
                    return new NOP();
            }
        }

        public static Instruction[] Parse(UInt32[] code)
        {
            Instruction[] result = new Instruction[code.Length];
            for (int i = 0; i < code.Length; i++)
                result[i] = Parse(code[i]);
            return result;
        }

        public class State
        {
            public UInt16[] memory = new UInt16[UInt16.MaxValue + 1];
            public UInt16[] registers = new UInt16[Enum.GetValues(typeof(Reg)).Length];
            public UInt16 counter = 0;
        }

        public State _state = new State();

        public void Run(UInt32[] code)
        {
            Instruction[] program = Parse(code);
            while (_state.counter < UInt16.MaxValue)
            {
                program[_state.counter].Execute(_state);
            }
        }
    }

    class Mainer
    {
        class WorkerArgs
        {
            public Program p;
            public UInt32[] c;
            public ManualResetEvent starter = new ManualResetEvent(false);
            public ManualResetEvent ender = new ManualResetEvent(false);
        }

        static void Worker(object o)
        {
            WorkerArgs a = o as WorkerArgs;
        beginning:
            try
            {
                a.starter.WaitOne();
                if (a.p == null)
                    return;
                a.p.Run(a.c);
                a.ender.Set();
            }
            catch (ThreadAbortException)
            {
                Thread.ResetAbort();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
            finally
            {
                a.starter.Reset();
            }
            goto beginning;
        }

        static void Main(string[] args)
        {
            /*
            Program prog = new Program();
            prog._state.memory[0] = 6;
            prog._state.memory[1] = 3;
            prog._state.memory[2] = 2;
            prog._state.memory[3] = 9;
            prog._state.memory[4] = 0;

            string text =
                "SET R0 0\r\n" +
                "SET R1 3\r\n" +
                "SET R2 0\r\n" +
                "BLT R0 R1 5\r\n" +
                "BEQ R0 R0 10\r\n" +
                "MRD R0 R3\r\n" +
                "ADD R3 R2 R2\r\n" +
                "SET R3 1\r\n" +
                "ADD R3 R0 R0\r\n" +
                "BEQ R0 R0 3\r\n" +
                "SET R3 4\r\n" +
                "MWT R3 R2";

            UInt32[] code = Program.Codify(text);
            prog.Run(code);
             */

            int successes = 0;
            Random r = new Random();
            UInt16[] baseValues = new UInt16[32];
            UInt16[] baseMemory = new UInt16[UInt16.MaxValue + 1];
            
            UInt32[][] codes = new UInt32[128][];
            Program[] progs = new Program[128];
            List<KeyValuePair<int, long>> fitnesses = new List<KeyValuePair<int, long>>();
            for (int i = 0; i < 128; i++)
            {
                progs[i] = new Program();
                codes[i] = new UInt32[UInt16.MaxValue];
                for (int j = 0; j < codes[i].Length; j++)
                    codes[i][j] = (UInt32)r.Next(0x20000000);
            }

            UInt32[][] kept = new UInt32[16][];
            for (int i = 0; i < 16; i++)
                kept[i] = new UInt32[UInt16.MaxValue];

            int c = 0;
            WorkerArgs a1 = new WorkerArgs(), a2 = new WorkerArgs(), a3 = new WorkerArgs(), a4 = new WorkerArgs();
            Thread t1 = new Thread(Worker);
            Thread t2 = new Thread(Worker);
            Thread t3 = new Thread(Worker);
            Thread t4 = new Thread(Worker);
            t1.Start(a1);
            t2.Start(a2);
            t3.Start(a3);
            t4.Start(a4);
            while (successes < 3)
            {
                c++;
                for (int i = 0; i < 32; i++)
                {
                    baseMemory[i] = baseValues[i] = (UInt16)r.Next(UInt16.MaxValue);
                }
                Array.Sort<UInt16>(baseValues);

                fitnesses.Clear();
                for (int i = 0; i < 32; i++)
                {
                    Array.Copy(baseMemory, progs[4 * i]._state.memory, baseMemory.Length);
                    Array.Copy(baseMemory, progs[4 * i + 1]._state.memory, baseMemory.Length);
                    Array.Copy(baseMemory, progs[4 * i + 2]._state.memory, baseMemory.Length);
                    Array.Copy(baseMemory, progs[4 * i + 3]._state.memory, baseMemory.Length);
                    a1.p = progs[4 * i];
                    a1.c = codes[4 * i];
                    a2.p = progs[4 * i + 1];
                    a2.c = codes[4 * i + 1];
                    a3.p = progs[4 * i + 2];
                    a3.c = codes[4 * i + 2];
                    a4.p = progs[4 * i + 3];
                    a4.c = codes[4 * i + 3];

                    a1.starter.Set();
                    a2.starter.Set();
                    a3.starter.Set();
                    a4.starter.Set();
                    if (!a1.ender.WaitOne(500))
                        t1.Abort();
                    if (!a2.ender.WaitOne(1))
                        t2.Abort();
                    if (!a3.ender.WaitOne(1))
                        t3.Abort();
                    if (!a4.ender.WaitOne(1))
                        t4.Abort();
                    a1.ender.Reset();
                    a2.ender.Reset();
                    a3.ender.Reset();
                    a4.ender.Reset();

                    long fit1 = 0, fit2 = 0, fit3 = 0, fit4 = 0;
                    for (int j = 0; j < 32; j++)
                    {
                        fit1 += (baseValues[j] - progs[4 * i]._state.memory[j]) * (baseValues[j] - progs[4 * i]._state.memory[j]);
                        fit2 += (baseValues[j] - progs[4 * i + 1]._state.memory[j]) * (baseValues[j] - progs[4 * i + 1]._state.memory[j]);
                        fit3 += (baseValues[j] - progs[4 * i + 2]._state.memory[j]) * (baseValues[j] - progs[4 * i + 2]._state.memory[j]);
                        fit4 += (baseValues[j] - progs[4 * i + 3]._state.memory[j]) * (baseValues[j] - progs[4 * i + 3]._state.memory[j]);
                    }
                    fitnesses.Add(new KeyValuePair<int, long>(4 * i, fit1));
                    fitnesses.Add(new KeyValuePair<int, long>(4 * i + 1, fit2));
                    fitnesses.Add(new KeyValuePair<int, long>(4 * i + 2, fit3));
                    fitnesses.Add(new KeyValuePair<int, long>(4 * i + 3, fit4));
                }

                fitnesses.Sort(delegate(KeyValuePair<int, long> a, KeyValuePair<int, long> b)
                {
                    return a.Value.CompareTo(b.Value);
                });

                if (fitnesses[0].Value == 0)
                    successes++;
                else
                    successes = 0;

                fitnesses.RemoveRange(16, 128 - 16);
                for (int i = 0; i < kept.Length; i++)
                    Array.Copy(codes[fitnesses[i].Key], kept[i], kept[i].Length);

                for (int i = 0; i < 64; i++)
                {
                    int id1 = r.Next(16), id2 = r.Next(16);
                    while (id1 == id2)
                        id2 = r.Next(16);
                    Array.Copy(kept[id1], codes[2 * i], kept[id1].Length);
                    Array.Copy(kept[id2], codes[2 * i + 1], kept[id2].Length);
                    
                    int nCrossovers = r.Next(5);
                    for (int j = 0; j < nCrossovers; j++)
                    {
                        int p1a = r.Next(UInt16.MaxValue);
                        int p1b = r.Next(UInt16.MaxValue);
                        if (p1a > p1b)
                        {
                            p1a ^= p1b;
                            p1b ^= p1a;
                            p1a ^= p1b;
                        }
                        for (int k = p1a; k < p1b; k++)
                        {
                            UInt32 v1 = codes[2 * i][k];
                            codes[2 * i][k] = codes[2 * i + 1][k];
                            codes[2 * i + 1][k] = v1;
                        }
                    }

                    int nMutations = r.Next(1024);
                    for (int j = 0; j < nMutations; j++)
                    {
                        int p1a = r.Next(UInt16.MaxValue);
                        int p1b = r.Next(29);
                        UInt32 mask = (UInt32)(1 << p1b);
                        codes[2 * i][p1a] ^= mask;

                        int p2a = r.Next(UInt16.MaxValue);
                        int p2b = r.Next(29);
                        mask = (UInt32)(1 << p2b);
                        codes[2 * i + 1][p2a] ^= mask;
                    }
                }
            }
            Console.ReadLine();
        }
    }
}
