package ATC_genCodegen;

import ATC_genModel.UmlModel;
import ATC_genModel.UmlType;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class JavaCodeGenerator {

    public static void generate(UmlModel model, Path outDir) throws IOException {
        Files.createDirectories(outDir);

        for (UmlType t : model.getTypes()) {
            if (t.name == null || t.name.trim().isEmpty()) continue;

            Path pkgDir = outDir;
            if (t.packageName != null && !t.packageName.trim().isEmpty()) {
                pkgDir = outDir.resolve(t.packageName.replace('.', '/'));
            }
            Files.createDirectories(pkgDir);

            Path javaFile = pkgDir.resolve(t.name + ".java");
            String code = renderType(model, t);
            Files.write(javaFile, code.getBytes(StandardCharsets.UTF_8));
        }
    }

    private static String renderType(UmlModel model, UmlType t) {
        StringBuilder sb = new StringBuilder();

        if (t.packageName != null && !t.packageName.trim().isEmpty()) {
            sb.append("package ").append(t.packageName).append(";\n\n");
        }

        Set<String> imports = computeImports(model, t);
        for (String imp : imports) sb.append("import ").append(imp).append(";\n");
        if (!imports.isEmpty()) sb.append("\n");

        if (t.kind == UmlType.Kind.CLASS) {
            renderClass(model, t, sb);
        } else if (t.kind == UmlType.Kind.INTERFACE) {
            renderInterface(model, t, sb);
        } else if (t.kind == UmlType.Kind.ENUM) {
            renderEnum(t, sb);
        } else {
            sb.append("// Unsupported type\n");
        }

        return sb.toString();
    }

    // ---------------- CLASS ----------------

    private static void renderClass(UmlModel model, UmlType t, StringBuilder sb) {

        boolean hasAbstractOwnedOps = false;
        for (UmlType.UmlOperation op : t.operations) {
            if (op.isAbstract) { hasAbstractOwnedOps = true; break; }
        }

        sb.append("public ");
        if (hasAbstractOwnedOps) sb.append("abstract ");
        sb.append("class ").append(t.name);

        String ext = resolveTypeName(model, t.superTypeId);
        if (ext != null && !ext.isEmpty() && !"Object".equals(ext)) {
            sb.append(" extends ").append(ext);
        }

        List<String> impl = new ArrayList<String>();
        for (String iid : t.interfaceIds) {
            String in = resolveTypeName(model, iid);
            if (in != null && !in.isEmpty() && !"Object".equals(in)) impl.add(in);
        }
        if (!impl.isEmpty()) sb.append(" implements ").append(join(impl, ", "));

        sb.append(" {\n\n");

        // attributes
        sb.append("    // class attributes\n");
        for (UmlType.UmlAttribute a : t.attributes) {
            String vis = mapVisibilityToJava(a.visibility, "private");
            sb.append("    ");
            if (vis.length() > 0) sb.append(vis).append(" ");
            sb.append(fieldType(model, a)).append(" ").append(a.name).append(";\n");
        }
        sb.append("\n\n");

        // constructor (tutti gli attributi)
        sb.append("    // constructors\n");
        if (!t.attributes.isEmpty()) {
            sb.append("    public ").append(t.name).append("(");
            for (int i = 0; i < t.attributes.size(); i++) {
                UmlType.UmlAttribute a = t.attributes.get(i);
                sb.append(fieldType(model, a)).append(" ").append(a.name);
                if (i < t.attributes.size() - 1) sb.append(", ");
            }
            sb.append(") {\n");
            for (UmlType.UmlAttribute a : t.attributes) {
                sb.append("        this.").append(a.name).append(" = ").append(a.name).append(";\n");
            }
            sb.append("    }\n\n");
        } else {
            sb.append("    public ").append(t.name).append("() {\n");
            sb.append("    }\n\n");
        }

        // getters/setters
        sb.append("    // SET and GET methods\n");
        for (UmlType.UmlAttribute a : t.attributes) {
            String ft = fieldType(model, a);
            String cap = capitalize(a.name);

            sb.append("    public ").append(ft).append(" get").append(cap).append("() {\n");
            sb.append("        return this.").append(a.name).append(";\n");
            sb.append("    }\n\n");

            sb.append("    public void set").append(cap).append("(").append(ft).append(" value) {\n");
            sb.append("        this.").append(a.name).append(" = value;\n");
            sb.append("    }\n\n");
        }

        // owned operations
        sb.append("    // class operations (owned)\n\n");
        for (UmlType.UmlOperation op : t.operations) {
            if (op.isAbstract) {
                sb.append("    ").append(mapVisibilityToJava(op.visibility, "public")).append(" abstract ")
                  .append(javaTypeForReturn(model, op)).append(" ").append(op.name)
                  .append(renderParamList(model, op)).append(";\n\n");
            } else {
                sb.append(renderOperationStub(model, op, false, t.name)).append("\n");
            }
        }

        // operations to implement (interfaces + abstract inherited)
        sb.append("    // operations to implement (interfaces + abstract inherited)\n\n");
        Map<String, UmlType.UmlOperation> toImpl = collectOperationsToImplement(model, t);

        Set<String> ownedSigs = new HashSet<String>();
        for (UmlType.UmlOperation op : t.operations) ownedSigs.add(signature(model, op));

        for (UmlType.UmlOperation op : toImpl.values()) {
            if (ownedSigs.contains(signature(model, op))) continue;
            sb.append(renderOperationStub(model, op, true, t.name)).append("\n");
        }

        sb.append("}\n");
    }

    // ---------------- INTERFACE ----------------

    private static void renderInterface(UmlModel model, UmlType t, StringBuilder sb) {
        sb.append("public interface ").append(t.name).append(" {\n\n");
        for (UmlType.UmlOperation op : t.operations) {
            // in Java i metodi di interfaccia sono public di default, ma lo scrivo per chiarezza
            sb.append("    public ").append(javaTypeForReturn(model, op)).append(" ").append(op.name)
              .append(renderParamList(model, op)).append(";\n\n");
        }
        sb.append("}\n");
    }

    // ---------------- ENUM ----------------

    private static void renderEnum(UmlType t, StringBuilder sb) {
        sb.append("public enum ").append(t.name).append(" {\n");
        if (!t.enumLiterals.isEmpty()) {
            for (int i = 0; i < t.enumLiterals.size(); i++) {
                sb.append("    ").append(t.enumLiterals.get(i));
                sb.append(i < t.enumLiterals.size() - 1 ? ",\n" : ";\n");
            }
        } else {
            sb.append("    ;\n");
        }
        sb.append("}\n");
    }

    // ---------------- operations helpers ----------------

    private static String renderParamList(UmlModel model, UmlType.UmlOperation op) {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        for (int i = 0; i < op.parameters.size(); i++) {
            UmlType.UmlParameter p = op.parameters.get(i);
            sb.append(javaTypeForParam(model, p)).append(" ").append(safeParamName(p.name, i));
            if (i < op.parameters.size() - 1) sb.append(", ");
        }
        sb.append(")");
        return sb.toString();
    }

    private static String renderOperationStub(UmlModel model, UmlType.UmlOperation op, boolean addOverride, String className) {
        StringBuilder sb = new StringBuilder();

        if (addOverride) sb.append("    @Override\n");

        String visibility = mapVisibilityToJava(op.visibility, "public");
        String ret = javaTypeForReturn(model, op);

        sb.append("    ").append(visibility).append(" ");
        if (op.isStatic) sb.append("static ");

        sb.append(ret).append(" ").append(op.name).append(renderParamList(model, op)).append(" {\n\n");

        sb.append("        //Start of user code ").append(className).append("_body : method body preserved\n");
        sb.append("        // TODO: implementazione\n");
        sb.append("        //End of user code\n");

        if (!"void".equals(ret)) {
            sb.append("\n        return ").append(defaultReturnFor(ret)).append(";\n");
        }

        sb.append("    }\n");
        return sb.toString();
    }

    private static Map<String, UmlType.UmlOperation> collectOperationsToImplement(UmlModel model, UmlType clazz) {
        Map<String, UmlType.UmlOperation> result = new LinkedHashMap<String, UmlType.UmlOperation>();

        // interface ops
        for (String iid : clazz.interfaceIds) {
            UmlType it = model.findById(iid);
            if (it != null) {
                for (UmlType.UmlOperation op : it.operations) {
                    result.put(signature(model, op), op);
                }
            }
        }

        // abstract inherited ops (super chain)
        String sup = clazz.superTypeId;
        while (sup != null && sup.trim().length() > 0) {
            UmlType s = model.findById(sup);
            if (s == null) break;

            for (UmlType.UmlOperation op : s.operations) {
                if (op.isAbstract) result.put(signature(model, op), op);
            }
            sup = s.superTypeId;
        }

        return result;
    }

    //firma basata sui tipi "java finali" (List<...> inclusa), così gli override matchano
    private static String signature(UmlModel model, UmlType.UmlOperation op) {
        StringBuilder sb = new StringBuilder();
        sb.append(op.name).append("(");
        for (int i = 0; i < op.parameters.size(); i++) {
            sb.append(javaTypeForParam(model, op.parameters.get(i)));
            if (i < op.parameters.size() - 1) sb.append(",");
        }
        sb.append(")->").append(javaTypeForReturn(model, op));
        return sb.toString();
    }

    private static String safeParamName(String name, int idx) {
        if (name != null && name.trim().length() > 0) return name;
        return "p" + idx;
    }

    private static String defaultReturnFor(String javaType) {
        if ("boolean".equals(javaType) || "Boolean".equals(javaType)) return "false";
        if ("int".equals(javaType) || "Integer".equals(javaType)) return "0";
        if ("double".equals(javaType) || "Double".equals(javaType)) return "0.0";
        return "null"; // anche per List<...> (stile Acceleo)
    }

    // tipi e import

    private static Set<String> computeImports(UmlModel model, UmlType t) {
        Set<String> imports = new TreeSet<String>();

        boolean needsList = false;

        // attributes
        for (UmlType.UmlAttribute a : t.attributes) {
            if (a.isMany()) needsList = true;
        }

        // owned operations
        for (UmlType.UmlOperation op : t.operations) {
            if (op.returnTypeRef != null && op.returnTypeRef.trim().length() > 0 && op.returnIsMany()) needsList = true;
            for (UmlType.UmlParameter p : op.parameters) {
                if (isManyUpper(p.upper)) needsList = true;
            }
        }

        // interfacce e abstract inherited operations
        if (t.kind == UmlType.Kind.CLASS) {
            Map<String, UmlType.UmlOperation> toImpl = collectOperationsToImplement(model, t);
            for (UmlType.UmlOperation op : toImpl.values()) {
                if (op.returnTypeRef != null && op.returnTypeRef.trim().length() > 0 && op.returnIsMany()) needsList = true;
                for (UmlType.UmlParameter p : op.parameters) {
                    if (isManyUpper(p.upper)) needsList = true;
                }
            }
        }

        if (needsList) imports.add("java.util.List");

        // attribute types
        for (UmlType.UmlAttribute a : t.attributes) addImportIfNeeded(model, t, a.typeRef, imports);

        // super and interfaces
        addImportIfNeeded(model, t, t.superTypeId, imports);
        for (String iid : t.interfaceIds) addImportIfNeeded(model, t, iid, imports);

        // owned operations types
        for (UmlType.UmlOperation op : t.operations) {
            addImportIfNeeded(model, t, op.returnTypeRef, imports);
            for (UmlType.UmlParameter p : op.parameters) addImportIfNeeded(model, t, p.typeRef, imports);
        }

        // interface/abstract inherited types
        if (t.kind == UmlType.Kind.CLASS) {
            Map<String, UmlType.UmlOperation> toImpl = collectOperationsToImplement(model, t);
            for (UmlType.UmlOperation op : toImpl.values()) {
                addImportIfNeeded(model, t, op.returnTypeRef, imports);
                for (UmlType.UmlParameter p : op.parameters) addImportIfNeeded(model, t, p.typeRef, imports);
            }
        }

        return imports;
    }

    private static void addImportIfNeeded(UmlModel model, UmlType current, String typeRef, Set<String> imports) {
        if (typeRef == null || typeRef.trim().isEmpty()) return;
        if (typeRef.contains("#")) return; 

        UmlType ref = model.findById(typeRef.trim());
        if (ref == null) return;

        if (current.packageName != null && !current.packageName.trim().isEmpty()
                && ref.packageName != null && ref.packageName.equals(current.packageName)) {
            return;
        }

        if (ref.packageName != null && !ref.packageName.trim().isEmpty()) {
            imports.add(ref.packageName + "." + ref.name);
        }
    }

    private static String fieldType(UmlModel model, UmlType.UmlAttribute a) {
        String base = resolveTypeName(model, a.typeRef);
        if (base == null || base.trim().isEmpty()) base = "Object";
        if (a.isMany()) return "List<" + base + ">";
        return base;
    }

    // PARAM: se molteplicità è many => List<T>
    private static String javaTypeForParam(UmlModel model, UmlType.UmlParameter p) {
        String base = resolveTypeName(model, p.typeRef);
        if (base == null || base.trim().isEmpty()) base = "Object";
        if (isManyUpper(p.upper)) return "List<" + base + ">";
        return base;
    }

    // RETURN: se molteplicità è many => List<T>
    private static String javaTypeForReturn(UmlModel model, UmlType.UmlOperation op) {
        if (op.returnTypeRef == null || op.returnTypeRef.trim().isEmpty()) return "void";
        String base = resolveTypeName(model, op.returnTypeRef);
        if (base == null || base.trim().isEmpty()) return "void";
        if (op.returnIsMany()) return "List<" + base + ">";
        return base;
    }

    // many = "*" / "-1" / oppure upper numerico > 1
    private static boolean isManyUpper(String upper) {
        if (upper == null || upper.trim().isEmpty()) return false;
        upper = upper.trim();
        if ("*".equals(upper) || "-1".equals(upper)) return true;
        try {
            return Integer.parseInt(upper) > 1;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private static String resolveTypeName(UmlModel model, String typeRef) {
        if (typeRef == null || typeRef.trim().isEmpty()) return "Object";
        typeRef = typeRef.trim();

        int idx = typeRef.lastIndexOf('#');
        if (idx >= 0 && idx < typeRef.length() - 1) {
            String prim = typeRef.substring(idx + 1);
            return mapPrimitive(prim);
        }

        UmlType t = model.findById(typeRef);
        if (t != null) return t.name;

        return "Object";
    }

    private static String mapPrimitive(String umlName) {
        if (umlName == null) return "Object";
        if ("String".equals(umlName)) return "String";
        if ("Integer".equals(umlName)) return "Integer";
        if ("Real".equals(umlName)) return "Double";
        if ("Boolean".equals(umlName)) return "Boolean";
        if ("UnlimitedNatural".equals(umlName)) return "Integer";
        return umlName;
    }

    private static String mapVisibilityToJava(String umlVisibility, String defaultVis) {
        String v = umlVisibility;
        if (v == null || v.trim().isEmpty()) v = defaultVis;
        v = v.trim();

        if ("package".equals(v)) return "";
        if ("public".equals(v) || "private".equals(v) || "protected".equals(v)) return v;
        return defaultVis;
    }

    private static String capitalize(String s) {
        if (s == null || s.isEmpty()) return s;
        if (s.length() == 1) return s.toUpperCase();
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    private static String join(List<String> items, String sep) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < items.size(); i++) {
            sb.append(items.get(i));
            if (i < items.size() - 1) sb.append(sep);
        }
        return sb.toString();
    }
}
