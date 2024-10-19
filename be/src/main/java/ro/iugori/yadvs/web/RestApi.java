package ro.iugori.yadvs.web;

public interface RestApi {

    interface URI {

        String ROOT = "/yadvs/rest/v1";

        interface Polls {
            String ID = "/polls";
            String ROOT = URI.ROOT + ID;
        }
    }

    interface Header {

        String PREFER = "Prefer";
        String PREFERENCE_APPLIED = "Preference-Applied";

        interface Value {
            String RETURN_MINIMAL = "return=minimal";
            String RETURN_REPRESENTATION = "return=representation";
        }
    }

    interface Param {
        String FIELDS = "~fields";
        String SORT = "~sort";
        String PAGE_NO = "~pageNo";
        String PAGE_SIZE = "~pageSize";
    }

}
