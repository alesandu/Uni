package service;

import java.util.List;

public interface IRadarSurveillance {

    public List<RadarTrack> acquireTracks();

    public Integer getUpdatePeriodMs();

}
