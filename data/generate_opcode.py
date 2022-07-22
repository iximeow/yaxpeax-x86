import json

isa_data = json.loads(open("./x86_64.json").read())

class IsaData:
    def __init__(self, data):
        isa_extensions = isa_data['sets']
        microarchitectures = isa_data['uarch']

        loaded_exts = {}
        for ext in isa_extensions:
            loaded_exts[ext['name']] = ext

        loaded_uarches = {}
        for arch in microarchitectures:
            loaded_uarches[arch['name']] = arch

        self.exts = loaded_exts
        self.uarches = loaded_uarches
        self.referenced_sets = set([])

    def compile_uarch(self, arch):
        uarch = self.uarches[arch]
        sets = self.uarch_sets(arch)

        out = {
            "name": uarch['name'],
            "vendor": uarch['vendor'],
            "extensions": list(sets),
            "instructions": list(self.sets2instructions(sets))
        }

        return out

    def uarch_sets(self, arch):
        sets = set([])
        uarch = self.uarches[arch]

        for item in uarch['sets']:
            if item[0] == '/':
                continue
            elif item[0] == '+':
                sets |= self.uarch_sets(item[1:])
            else:
                sets.add(item)

        sets = self.resolve_sets(sets)

        self.referenced_sets |= sets
        return sets

    def unused_extensions(self):
        all_sets = set(self.exts.keys())
        return all_sets - self.referenced_sets

    def used_extensions(self):
        return self.referenced_sets

    def sets2instructions(self, sets):
        instructions = set([])
        for ext in sets:
            ext = self.exts[ext]
            for item in ext['new']:
                if item[0] != '+':
                    instructions.add(item)
        return instructions

    def resolve_sets(self, sets):
        resolved_sets = set(list(sets))
        self.referenced_sets |= resolved_sets

        for ext in sets:
            ext = self.exts[ext]
            for item in ext['new']:
                if item[0] == '/':
                    continue
                elif item[0] == '+':
                    new_sets = self.resolve_sets(set([item[1:]]))
                    self.referenced_sets |= new_sets
                    resolved_sets |= new_sets

        return resolved_sets

isa_data = IsaData(isa_data)

for (arch, data) in isa_data.uarches.items():
    arch = isa_data.compile_uarch(arch)
#    print("x: {}".format(json.dumps(arch)))

print("unused sets: {}".format(json.dumps(list(isa_data.unused_extensions()))))
print("used sets: {}".format(json.dumps(list(isa_data.used_extensions()))))

ROOTS = ["real_mode", "protected_mode", "long_mode", "x86_generic"]

ALL_INSTRUCTIONS = set([])
INSTRUCTION_NUMS = {}

for root in ROOTS:
    arch = isa_data.compile_uarch(root)
    print("name: {}, {} instructions".format(arch['name'], len(arch['instructions'])))
    ALL_INSTRUCTIONS |= set(arch['instructions'])

print("opcode enum to follow...")

class Output:
    def __init__(self, f):
        self.indentation = 0
        self.out = f
        self.should_indent = False

    def begin_block(self, content):
        self.write(content)
        self.write(" {")
        self.newline()
        self.indent()

    def end_block(self):
        self.outdent()
        self.write("}")
        self.newline()

    def indent(self):
        self.indentation += 1

    def outdent(self):
        self.indentation -= 1

    def write(self, content):
        if self.should_indent:
            self.out.write("  " * self.indentation)
            self.should_indent = False
        self.out.write(content)

    def newline(self):
        self.out.write("\n")
        self.should_indent = True

    def comment(self, content):
        if not self.should_indent:
            # we've written something on this line. add a space to be reader-friendly.
            self.out.write(" ")
        self.write("// ")
        self.writeline(content)

    def writeline(self, content):
        self.write(content)
        self.newline()

    def close(self):
        self.out.close()

OPCODE_REPR = "u16"
OPCODE_ANNOTATIONS = [
    "#[allow(non_camel_case_types)]",
    "#[derive(Copy, Clone, Debug, Eq, PartialEq)]",
    "#[non_exhaustive]",
    "#[repr({})]".format(OPCODE_REPR)
]

f = open("../src/generated/mod.rs", "w")
f = Output(f)
f.writeline("pub(crate) mod opcode;")
f.writeline("pub(crate) mod imp;")
f.newline()

for root in ROOTS:
    if root == "x86_generic":
        continue
    f.begin_block("pub(crate) mod {}".format(root))
    f.writeline("pub(crate) use super::opcode::{}::Opcode as Opcode;".format(root))
    f.writeline("pub(crate) use super::imp::{}::revise_instruction as revise_instruction;".format(root))
    f.end_block()

f = open("../src/generated/opcode.rs", "w")
f = Output(f)
for annotation in OPCODE_ANNOTATIONS:
    f.writeline(annotation)
f.begin_block("pub enum Opcode")

insts = list(ALL_INSTRUCTIONS)
insts.sort()
for (i, inst) in enumerate(insts):
    INSTRUCTION_NUMS[inst] = i
    if inst == "invalid":
        f.writeline("Invalid,")
    else:
        f.writeline("{},".format(str(inst).upper()))
f.end_block()

f.newline()

f.writeline("pub(crate) const MNEMONICS: &'static [&'static str] = &[")
f.indent()
for (_i, inst) in enumerate(insts):
    f.writeline('"{}",'.format(inst.lower()))
f.outdent()
f.writeline("];")
f.newline()

for root in ROOTS:
    if root == "x86_generic":
        continue
    f.begin_block("pub(crate) mod {}".format(root))

    for annotation in OPCODE_ANNOTATIONS:
        f.writeline(annotation)
    f.begin_block("pub enum Opcode")
    arch = isa_data.compile_uarch(root)
    insts = arch['instructions']
    insts.sort()
    for inst in insts:
        if inst == "invalid":
            f.writeline("Invalid = super::Opcode::Invalid as {},".format(OPCODE_REPR))
        else:
            f.writeline("{} = super::Opcode::{} as {},".format(str(inst).upper(), str(inst).upper(), OPCODE_REPR))
    f.end_block()
    f.end_block()

    f.newline()

f = open("../src/generated/imp.rs", "w")
f = Output(f)

# f.writeline("fn main() {}\n")

#    f.comment("should be `{}::DecodeError` but i want to compile this on its own while bootstrapping")

#    f.writeline('#[path="/toy/yaxpeax/arch/x86/src/{}/mod.rs"]'.format(root))
#    f.writeline("mod structs;")
#    f.writeline("use structs::{InstDecoder, Instruction};")

#    f.begin_block("enum DecodeError")
#    f.writeline("InvalidOpcode,")
#    f.end_block()

#    f.newline()


for root in ROOTS:
    if root == "x86_generic":
        continue
    arch = isa_data.compile_uarch(root)

    f.begin_block("pub(crate) mod {}".format(root))
    f.writeline("use crate::generated::{}::Opcode;".format(root))
    f.writeline("use crate::{}::{{InstDecoder, Instruction, DecodeError}};".format(root))

    f.begin_block("impl InstDecoder")
    for ext in arch['extensions']:
        f.begin_block("fn feature_{}(&self) -> bool".format(ext))
        f.writeline("true")
        f.end_block()
    f.end_block()

    f.begin_block("pub(crate) fn revise_instruction(decoder: &InstDecoder, inst: &mut Instruction) -> Result<(), DecodeError>")

    f.begin_block("if inst.prefixes.evex().is_some()")
    f.begin_block("if !decoder.avx512()")
    f.writeline("return Err(DecodeError::InvalidOpcode);")
    f.outdent()
    f.writeline("} else {")
    f.indent()
    f.writeline("return Ok(());")
    f.end_block()
    f.end_block()

    f.newline()

    f.comment("for some instructions (tzcnt), not having an extension means the instruction is")
    f.comment("interpreted as another, rather than being simply rejected.")
    f.comment("we might still reject the alternate instruction later, if the extension adding *it*")
    f.comment("is also not supported.")
    f.begin_block("if inst.opcode == Opcode::TZCNT")
    f.begin_block("if !decoder.bmi1()")
    f.comment("tzcnt is only supported if bmi1 is enabled. without bmi1, this decodes as bsf.")
    f.writeline("inst.opcode = Opcode::BSF;")
    f.end_block()
    f.end_block()

    f.newline()

    f.begin_block("match inst.opcode")
    f.comment("we'll never be rejecting the instruction `Invalid`")
    f.writeline("Opcode::Invalid => {}")
    for ext in arch['extensions']:
        ext_data = isa_data.exts[ext]['new']
        suffix_needed = False
        any_entries = False
        for (i, inst) in enumerate(ext_data):
            if inst[0] == '+':
                continue

            # special-cased `invalid` above; it's always present.
            if inst == "invalid":
                continue

            if suffix_needed:
                f.writeline("|")

            f.write("Opcode::{} ".format(inst.upper()))
            suffix_needed = True
            any_entries = True
        if not any_entries:
            print("no entries for ext {}".format(ext))
            continue
        f.begin_block("=>")
        f.begin_block("if !decoder.feature_{}()".format(ext))
        f.writeline("return Err(DecodeError::InvalidOpcode);\n")
        f.end_block()
        f.end_block()
    f.end_block()
    f.writeline("Ok(())")
    f.end_block()
    f.end_block()
    f.newline()

f.close()

