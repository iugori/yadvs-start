package ro.yugori.yadvs.api;

public interface MoreHttpHeaders {

    String PREFER = "Prefer";
    String PREFERENCE_APPLIED = "Preference-Applied";

    interface Values {
        String RETURN_MINIMAL = "return=minimal";
        String RETURN_REPRESENTATION = "return=representation";
    }

}

