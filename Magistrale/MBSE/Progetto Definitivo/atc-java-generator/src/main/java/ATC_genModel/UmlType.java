package ATC_genModel;

import java.util.ArrayList;
import java.util.List;

public class UmlType {

    public enum Kind { CLASS, INTERFACE, ENUM }

    public String id;
    public String name;
    public String packageName; // es: comms, core, domain...
    public Kind kind;

    // extends / implements 
    public String superTypeId; 
    public final List<String> interfaceIds = new ArrayList<>(); // interfaceRealization 

    // Attributi (ownedAttribute)
    public final List<UmlAttribute> attributes = new ArrayList<>();

    // Enum literals (ownedLiteral)
    public final List<String> enumLiterals = new ArrayList<>();

    // Operazioni (ownedOperation)
    public final List<UmlOperation> operations = new ArrayList<>();

    public String qualifiedName() {
        if (packageName == null || packageName.trim().isEmpty()) return name;
        return packageName + "." + name;
    }

    //helper generale per la molteplicità 
    public static boolean isManyUpper(String upper) {
        if (upper == null || upper.trim().isEmpty()) return false;
        upper = upper.trim();
        if ("*".equals(upper) || "-1".equals(upper)) return true;
        try {
            return Integer.parseInt(upper) > 1; 
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static class UmlAttribute {
        public String name;
        public String visibility; 

        public String typeRef;

        public int lower = 1;
        public String upper = "1";

        public boolean isMany() {
            return UmlType.isManyUpper(upper);
        }
    }

    public static class UmlOperation {
        public String name;
        public String visibility; 
        public boolean isAbstract;
        public boolean isStatic;

        public String returnTypeRef;
        public int returnLower = 1;
        public String returnUpper = "1";

        public boolean returnIsMany() {
            return UmlType.isManyUpper(returnUpper);
        }

        public final List<UmlParameter> parameters = new ArrayList<>();
    }

    public static class UmlParameter {
        public String name;
        public String direction;
        public String typeRef;

        public int lower = 1;
        public String upper = "1";

        public boolean isMany() {
            return UmlType.isManyUpper(upper);
        }
    }
}
