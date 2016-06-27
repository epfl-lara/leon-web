package leon.web.websitebuilder.state

import leon.purescala.Definitions.Program

/**
  * Created by dupriez on 27/06/16.
  */
abstract class SourceCodeAndProgramBinding(val sourceCode: String, val program: Program)
// StronglyBinded means that the program is "as if" it was the result of the compilation of the source code.
// In particular, the RangePosition of its Identifier for StringLiterals must correspond to the positions of the
//  corresponding StringLiterals in the source code.
class SourceCodeAndProgram_StronglyBinded(sourceCode: String, program: Program) extends SourceCodeAndProgramBinding(sourceCode, program)
// WeaklyBinded means that the program's structure and evaluation result are "as if" it was the result of the compilation of the source code.
// The main difference with SourceCodeAndProgram_StronglyBinded is that the RangePosition of the Identifier for
//  StringLiterals in program are not required to correspond to the positions of their corresponding StringLiteral in the source code.
class SourceCodeAndProgram_WeaklyBinded(sourceCode: String, program: Program) extends SourceCodeAndProgramBinding(sourceCode, program)
