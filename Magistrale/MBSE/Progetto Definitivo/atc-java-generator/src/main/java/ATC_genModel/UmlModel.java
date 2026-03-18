package ATC_genModel;

import java.util.*;

public class UmlModel {

    private final List<UmlType> types = new ArrayList<>();
    private final Map<String, UmlType> byId = new HashMap<>();

    public void addType(UmlType t) {
        types.add(t);
        if (t.id != null && !t.id.isEmpty()) {
            byId.put(t.id, t);
        }
    }

    public List<UmlType> getTypes() {
        return Collections.unmodifiableList(types);
    }

    public UmlType findById(String id) {
        if (id == null) return null;
        return byId.get(id);
    }
}
