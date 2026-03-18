package ATC_gen;

import ATC_genCodegen.JavaCodeGenerator;
import ATC_genModel.UmlModel;
import ATC_genParser.PapyrusUmlSaxParser;

import java.nio.file.Path;

public class Main {

    //classe main.java che inizia ed esegue la generazione
	
    public static void main(String[] args) throws Exception {

        String inputPath = null;            // null => usa resources/model/ATC_Model.uml
        String outputDir = "generated-src"; // default

        if (args.length == 1) {
            if (args[0].toLowerCase().endsWith(".uml")) {
                inputPath = args[0];
            } else {
                outputDir = args[0];
            }
        } else if (args.length >= 2) {
            inputPath = args[0];
            outputDir = args[1];
        }

        UmlModel model = PapyrusUmlSaxParser.parse(inputPath);

        System.out.println("=== TYPES FOUND ===");
        for (int i = 0; i < model.getTypes().size(); i++) {
            System.out.println(" - " + model.getTypes().get(i).kind + "  " + model.getTypes().get(i).qualifiedName());
        }

        JavaCodeGenerator.generate(model, Path.of(outputDir));

        System.out.println("=== DONE ===");
        System.out.println("Output folder: " + Path.of(outputDir).toAbsolutePath());
    }
}
