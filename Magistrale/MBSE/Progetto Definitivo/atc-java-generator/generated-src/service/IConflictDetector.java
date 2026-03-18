package service;

import domain.Aircraft;
import java.util.List;

public interface IConflictDetector {

    public List<Conflict> detect(List<Aircraft> aircrafts);

}
