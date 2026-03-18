package ATC_genParser;

import ATC_genModel.UmlModel;
import ATC_genModel.UmlType;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Iterator;

public class PapyrusUmlSaxParser {

    public static UmlModel parse(String inputPathOrNull) throws Exception {
        UmlModel model = new UmlModel();

        InputStream is;
        if (inputPathOrNull == null) {
            // classpath: src/main/resources/model/ATC_Model.uml
            is = PapyrusUmlSaxParser.class.getClassLoader().getResourceAsStream("model/ATC_Model.uml");
            if (is == null) {
                throw new IllegalStateException("File non trovato nel classpath: src/main/resources/model/ATC_Model.uml");
            }
        } else {
            is = Files.newInputStream(Paths.get(inputPathOrNull));
        }

        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);

        SAXParser parser = factory.newSAXParser();
        parser.parse(is, new Handler(model));
        return model;
    }

    private enum ElementKind { PACKAGE, TYPE, OTHER }

    private static class Handler extends DefaultHandler {

        private final UmlModel model;

        private final Deque<String> packageStack = new ArrayDeque<String>();
        private final Deque<ElementKind> packagedElementStack = new ArrayDeque<ElementKind>();

        private final Deque<UmlType> typeStack = new ArrayDeque<UmlType>();
        private final Deque<UmlType.UmlAttribute> attributeStack = new ArrayDeque<UmlType.UmlAttribute>();

        private final Deque<UmlType.UmlOperation> operationStack = new ArrayDeque<UmlType.UmlOperation>();
        private final Deque<UmlType.UmlParameter> parameterStack = new ArrayDeque<UmlType.UmlParameter>();

        Handler(UmlModel model) {
            this.model = model;
        }

        @Override
        public void startElement(String uri, String localName, String qName, Attributes atts) {
            String el = elementName(localName, qName);

            // ---------------- packagedElement: Package / Class / Interface / Enum ----------------
            if ("packagedElement".equals(el)) {
                String xmiType = atts.getValue("xmi:type"); // es: uml:Class
                String id = atts.getValue("xmi:id");
                String name = atts.getValue("name");

                if ("uml:Package".equals(xmiType)) {
                    packagedElementStack.push(ElementKind.PACKAGE);
                    packageStack.push(name != null ? name.trim() : "");
                    return;
                }

                UmlType.Kind kind = null;
                if ("uml:Class".equals(xmiType)) kind = UmlType.Kind.CLASS;
                else if ("uml:Interface".equals(xmiType)) kind = UmlType.Kind.INTERFACE;
                else if ("uml:Enumeration".equals(xmiType)) kind = UmlType.Kind.ENUM;

                if (kind != null) {
                    packagedElementStack.push(ElementKind.TYPE);

                    UmlType t = new UmlType();
                    t.id = id;
                    t.name = name;
                    t.kind = kind;
                    t.packageName = currentPackageQualified();

                    model.addType(t);
                    typeStack.push(t);
                } else {
                    packagedElementStack.push(ElementKind.OTHER);
                }
                return;
            }

            // ---------------- ownedAttribute ----------------
            if ("ownedAttribute".equals(el)) {
                if (!typeStack.isEmpty()) {
                    UmlType.UmlAttribute a = new UmlType.UmlAttribute();
                    a.name = atts.getValue("name");
                    a.visibility = atts.getValue("visibility"); // se vuoi rispettare public/private...

                    String typeAttr = atts.getValue("type");
                    if (typeAttr != null && typeAttr.trim().length() > 0) {
                        a.typeRef = typeAttr.trim();
                    }

                    attributeStack.push(a);
                }
                return;
            }

            // ---------------- ownedOperation ----------------
            if ("ownedOperation".equals(el)) {
                if (!typeStack.isEmpty()) {
                    UmlType.UmlOperation op = new UmlType.UmlOperation();
                    op.name = atts.getValue("name");
                    op.visibility = atts.getValue("visibility");
                    op.isAbstract = "true".equalsIgnoreCase(atts.getValue("isAbstract"));
                    op.isStatic = "true".equalsIgnoreCase(atts.getValue("isStatic"));
                    // returnTypeRef + returnLower/Upper verranno riempiti dal parametro direction="return"
                    operationStack.push(op);
                }
                return;
            }

            // ---------------- ownedParameter ----------------
            if ("ownedParameter".equals(el)) {
                if (!operationStack.isEmpty()) {
                    UmlType.UmlParameter p = new UmlType.UmlParameter();
                    p.name = atts.getValue("name"); 
                    p.direction = atts.getValue("direction"); 
                    if (p.direction == null || p.direction.trim().length() == 0) {
                        p.direction = "in";
                    }

                    String typeAttr = atts.getValue("type");
                    if (typeAttr != null && typeAttr.trim().length() > 0) {
                        p.typeRef = typeAttr.trim();
                    }

                    parameterStack.push(p);
                }
                return;
            }

            // ---------------- type (nested) ----------------
            // Papyrus spesso mette i primitive types così:
            // <type xmi:type="uml:PrimitiveType" href="pathmap://...#String"/>
            // oppure per tipi del modello: <type xmi:idref="..."/>
            // :contentReference[oaicite:2]{index=2}
            if ("type".equals(el)) {
                String href = atts.getValue("href");
                String idref = atts.getValue("xmi:idref");

                String ref = null;
                if (href != null && href.trim().length() > 0) ref = href.trim();
                else if (idref != null && idref.trim().length() > 0) ref = idref.trim();

                if (ref != null) {
                    // priorità: parametro > attributo
                    if (!parameterStack.isEmpty()) {
                        UmlType.UmlParameter p = parameterStack.peek();
                        if (p.typeRef == null || p.typeRef.trim().length() == 0) {
                            p.typeRef = ref;
                        }
                    } else if (!attributeStack.isEmpty()) {
                        UmlType.UmlAttribute a = attributeStack.peek();
                        if (a.typeRef == null || a.typeRef.trim().length() == 0) {
                            a.typeRef = ref;
                        }
                    }
                }
                return;
            }

            // ---------------- multiplicity (lowerValue / upperValue) ----------------
            // In Papyrus/UML2 la molteplicità sta spesso in lowerValue/upperValue, incluso value="*"
            if ("lowerValue".equals(el)) {
                String v = atts.getValue("value");
                if (v != null && v.trim().length() > 0) {
                    try {
                        int n = Integer.parseInt(v.trim());
                        if (!parameterStack.isEmpty()) parameterStack.peek().lower = n;
                        else if (!attributeStack.isEmpty()) attributeStack.peek().lower = n;
                    } catch (NumberFormatException ignored) { }
                }
                return;
            }

            if ("upperValue".equals(el)) {
                String v = atts.getValue("value");
                if (v != null && v.trim().length() > 0) {
                    if (!parameterStack.isEmpty()) parameterStack.peek().upper = v.trim();
                    else if (!attributeStack.isEmpty()) attributeStack.peek().upper = v.trim();
                }
                return;
            }

            // ---------------- extends ----------------
            if ("generalization".equals(el)) {
                if (!typeStack.isEmpty()) {
                    String generalId = atts.getValue("general");
                    if (generalId != null && generalId.trim().length() > 0) {
                        typeStack.peek().superTypeId = generalId.trim();
                    }
                }
                return;
            }

            // ---------------- implements ----------------
            if ("interfaceRealization".equals(el)) {
                if (!typeStack.isEmpty()) {
                    String contractId = atts.getValue("contract");
                    if (contractId != null && contractId.trim().length() > 0) {
                        typeStack.peek().interfaceIds.add(contractId.trim());
                    }
                }
                return;
            }

            // ---------------- enum literals ----------------
            if ("ownedLiteral".equals(el)) {
                if (!typeStack.isEmpty() && typeStack.peek().kind == UmlType.Kind.ENUM) {
                    String litName = atts.getValue("name");
                    if (litName != null && litName.trim().length() > 0) {
                        typeStack.peek().enumLiterals.add(litName.trim());
                    }
                }
            }
        }

        @Override
        public void endElement(String uri, String localName, String qName) {
            String el = elementName(localName, qName);

            if ("ownedAttribute".equals(el)) {
                if (!attributeStack.isEmpty() && !typeStack.isEmpty()) {
                    UmlType.UmlAttribute a = attributeStack.pop();
                    if (a.name != null && a.name.trim().length() > 0) {
                        typeStack.peek().attributes.add(a);
                    }
                }
                return;
            }

            if ("ownedParameter".equals(el)) {
                if (!parameterStack.isEmpty() && !operationStack.isEmpty()) {
                    UmlType.UmlParameter p = parameterStack.pop();
                    UmlType.UmlOperation op = operationStack.peek();

                    if ("return".equalsIgnoreCase(p.direction)) {
                        // tipo + molteplicità del RETURN (per List<...> in codegen)
                        op.returnTypeRef = p.typeRef;
                        op.returnLower = p.lower;
                        op.returnUpper = p.upper;
                    } else {
                        op.parameters.add(p);
                    }
                }
                return;
            }

            if ("ownedOperation".equals(el)) {
                if (!operationStack.isEmpty() && !typeStack.isEmpty()) {
                    UmlType.UmlOperation op = operationStack.pop();
                    if (op.name != null && op.name.trim().length() > 0) {
                        typeStack.peek().operations.add(op);
                    }
                }
                return;
            }

            if ("packagedElement".equals(el)) {
                if (!packagedElementStack.isEmpty()) {
                    ElementKind k = packagedElementStack.pop();
                    if (k == ElementKind.TYPE) {
                        if (!typeStack.isEmpty()) typeStack.pop();
                    } else if (k == ElementKind.PACKAGE) {
                        if (!packageStack.isEmpty()) packageStack.pop();
                    }
                }
            }
        }

        private String elementName(String localName, String qName) {
            if (localName != null && localName.trim().length() > 0) return localName;
            return qName;
        }

        private String currentPackageQualified() {
            if (packageStack.isEmpty()) return "";
            StringBuilder sb = new StringBuilder();
            Iterator<String> it = packageStack.descendingIterator();
            while (it.hasNext()) {
                String p = it.next();
                if (p == null || p.length() == 0) continue;
                if (sb.length() > 0) sb.append('.');
                sb.append(p);
            }
            return sb.toString();
        }
    }
}
