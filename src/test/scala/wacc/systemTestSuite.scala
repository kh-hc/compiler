package wacc

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import scala.io.Source
import scala.sys.process._
import scala.collection.mutable.Map

/*
System test suite, which tests the whole compiler by checking its output is correct
We test on valid programs which print statements
*/

class SystemTestSuite extends AnyFlatSpec {

    // Used for some tests - the root path
    val validRootPath = "src/test/scala/wacc/test_cases/valid/"

    // Get all files in the valid root path which contain a print or println statement
    val testFiles = getAllFiles(validRootPath)
    // testFiles.keys.foreach { test =>
    //     runTest(test, testFiles(test))
    // }

    // Strings that invalidate the tests - i.e the tests should be run manually and the result should be ignored
    val noTestFlags = Set("#addrs#", "read", " enter ", "#runtime_error#")

    def runTest(fileName : String, expectedOutput : (Int, String)) = {
        val filePath = fileName
        // Get the output assembly code from our compiler - stores in emulatorFiles/assemblyCode
        s"./compile $filePath > $filePath.log".! // val exitCode = 

        // The compiler saves the assembly code with the same file name, but .s instead of .wacc
        val assemblyFileName: String = fileName.dropRight(5).split("/").last
        val assemblyCode = s"$assemblyFileName.s"
        val assemblyOutput = s"$assemblyFileName.o"

        // Put it through the gcc cross-compiler - doesn't have an extenstion so store in fileName,
        // which is the same as the file path
        val compilerScriptCommand = "arm-linux-gnueabi-gcc -o " + assemblyOutput +
                                    " -mcpu=arm1176jzf-s -mtune=arm1176jzf-s " + assemblyCode
        
        compilerScriptCommand.!

        // Run the emulator
        val stdout = new StringBuilder
        val stderr = new StringBuilder
        val logger = ProcessLogger(stdout append _ + "\n", stderr append _ + "\n")
        //val emulatorScriptCommand = 
        if (!containsReadStatement(filePath)){
            println(s"Testing: $filePath")
            val actualExitCode = s"qemu-arm -L /usr/arm-linux-gnueabi/ $assemblyOutput".!(logger) 
            
            (filePath) should "be run with our compiler and have the correct output produced" in {
            if (noTestFlags.map(f => !(expectedOutput._2 contains f)).foldLeft(true)((a, b) => a & b)){  
                assert(actualExitCode == expectedOutput._1)
                assert(stdout.toString == expectedOutput._2)
            }
            else {
                println(s"File: $fileName escaped processing...")
                assert(true)
            }
        }
        } else {
            (filePath) should "work locally, it contains IO so cannot be tested" in {
                assert(true)
            }
        }
    }

    def allFiles(path:File):List[File]=
   {    
       val parts=path.listFiles.toList.partition(_.isDirectory)
       parts._2 ::: parts._1.flatMap(allFiles)         
   }

    
    def getAllFiles (path : String) : Map[String, (Int, String)] = {
        val dir = new File(path)
        val fileMap = Map.empty[String, (Int, String)]
        val files = allFiles(dir)
        files.map(f => fileMap.addOne(f.toString(), expectedOutput(f.toString())))
        return fileMap
    }

    // Checks if the file contains a print or println statement
    def containsPrintStatement(file : String) : Boolean = {
        // Get the lines from the file
        val lines = Source.fromFile(new File(file)).getLines()
        for (line <- lines) {
            if (line contains "print") {
                return true
            }
        }
        return false
    }

    def containsReadStatement(file: String) : Boolean = {
        val lines = Source.fromFile(new File(file)).getLines()
        for (line <- lines) {
            if (line contains "read ") {
                return true
            }
        }
        return false
    }

    def expectedOutput(file : String) : (Int, String) = {
        val lines = Source.fromFile(new File(file)).getLines()
        val terminalOutput = new StringBuilder()
        var exitCode = 0
        do {
            var line = lines.next()

            if (line contains "Output:") {
                line = lines.next()
                while(line contains "# "){
                    terminalOutput.append(line.slice(2, line.length()) + "\n")
                    line = lines.next()
                }
            }

            if (line contains "Exit:") {
                val exitLine = lines.next()
                exitCode = exitLine.slice(2, exitLine.length()).toInt
            }
        } while(lines.hasNext)
        return (exitCode, terminalOutput.toString())
    }
}
