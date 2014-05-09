using System;
using System.Collections.Generic;
using System.Text;

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
        public sealed override void Execute(Program.State state)
        {
            bool finished = false;
            TryExecute(state, ref finished);
            if (!finished)
                state.counter++;
        }

        protected virtual void TryExecute(Program.State state, ref bool finished)
        {
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
    }

    sealed class BEQ : BranchCompare
    {
        public BEQ(Reg rx, Reg ry, UInt16 addr) : base(rx, ry, addr) {}

        protected override bool Compare(Program.State state)
        {
            return state.registers[_rx] == state.registers[_ry];
        }
    }

    sealed class BLT : BranchCompare
    {
        public BLT(Reg rx, Reg ry, UInt16 addr) : base(rx, ry, addr) { }

        protected override bool Compare(Program.State state)
        {
            return state.registers[_rx] < state.registers[_ry];
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
            state.registers[_rz] = Operation(state.registers[_rx], state.registers[_ry]);
        }
    }

    sealed class ADD : BinaryOperation
    {
        public ADD(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 + v2;
        }
    }

    sealed class SUB : BinaryOperation
    {
        public SUB(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 - v2;
        }
    }

    sealed class MLT : BinaryOperation
    {
        public MLT(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 * v2;
        }
    }

    sealed class MOD : BinaryOperation
    {
        public MOD(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 % v2;
        }
    }

    sealed class AND : BinaryOperation
    {
        public AND(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 & v2;
        }
    }

    sealed class LOR : BinaryOperation
    {
        public LOR(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 | v2;
        }
    }

    sealed class XOR : BinaryOperation
    {
        public XOR(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 ^ v2;
        }
    }

    sealed class SHR : BinaryOperation
    {
        public SHR(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 >> v2;
        }
    }

    sealed class SHL : BinaryOperation
    {
        public SHL(Reg rx, Reg ry, Reg rz) : base(rx, ry, rz) { }

        protected override UInt16 Operation(UInt16 v1, UInt16 v2)
        {
            return v1 << v2;
        }
    }

    sealed class NEG : NOP
    {
        private Reg _rx;
        public NEG(Reg rx)
        {
            _rx = rx;
        }

        protected override void TryExecute(Program.State state, ref bool finished)
        {
            state.registers[_rx] = ~state.registers[_rx];
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
            state.registers[_ry] = state.registers[_rx];
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
            state.registers[_rx] = _value;
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
            state.registers[_ry] = state.memory[(int)state.registers[_rx]];
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
            state.memory[(int)state.registers[_rx]] = state.registers[_ry];
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
            InstructionSet["BEQ"] = 0x01000000;
            InstructionSet["BLT"] = 0x02000000;
            InstructionSet["AND"] = 0x10000000;
            InstructionSet["LOR"] = 0x11000000;
            InstructionSet["XOR"] = 0x12000000;
            InstructionSet["SHR"] = 0x13000000;
            InstructionSet["SHL"] = 0x14000000;
            InstructionSet["NEG"] = 0x15000000;
            InstructionSet["ADD"] = 0x20000000;
            InstructionSet["SUB"] = 0x21000000;
            InstructionSet["MLT"] = 0x22000000;
            InstructionSet["MOD"] = 0x23000000;
            InstructionSet["CPY"] = 0x30000000;
            InstructionSet["SET"] = 0x31000000;
            InstructionSet["MRD"] = 0x32000000;
            InstructionSet["MWT"] = 0x33000000;

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
                return;

            UInt32 instruction = InstructionSet[seg[0]];
            switch (seg[0])
            {
                case "BEQ":
                case "BLT":
                    instruction |= (RegisterSet[seg[1]] << 12);
                    instruction |= (RegisterSet[seg[2]] << 8);
                    instruction |= int.Parse(seg[3]);
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
                    instruction |= (RegisterSet[seg[1]] << 8);
                    instruction |= (RegisterSet[seg[2]] << 4);
                    instruction |= RegisterSet[seg[3]];
                    break;
                case "NEG":
                    instruction |= RegisterSet[seg[1]];
                    break;
                case "CPY":
                case "MRD":
                case "MWT":
                    instruction |= (RegisterSet[seg[1]] << 4);
                    instruction |= RegisterSet[seg[2]];
                    break;
                case "SET":
                    instruction |= (RegisterSet[seg[1]] << 8);
                    instruction |= int.Parse(seg[2]);
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
            UInt16 value = code | 0xffff;
            Reg rp = (code | 0x00f00000) >> 20;
            Reg rq = (code | 0x000f0000) >> 16;
            Reg rr = (code | 0x00000f00) >> 8;
            Reg rs = (code | 0x0000000f);
            switch (InstructionLookup[InstructionMask | code])
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
                    return new NEG(rs);
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

        public struct State
        {
            public UInt16[] memory = new UInt16[UInt16.MaxValue];
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
        static void Main(string[] args)
        {
            Program prog = new Program();
            string text = "";
        }
    }
}
