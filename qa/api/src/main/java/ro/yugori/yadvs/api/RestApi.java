package ro.yugori.yadvs.api;

public interface RestApi {

    interface URI {

        String ROOT = "/yadvs/rest/v1";
        String WILDCARD = "-";

        interface Polls {
            String ID = "/polls";
            String ROOT = URI.ROOT + ID;
        }
    }

    interface Header {

        String ACCEPT_LINKS = "Accept-Links";
        String PREFER = "Prefer";
        String PREFERENCE_APPLIED = "Preference-Applied";
        String X_CORRELATION_ID = "X-Correlation-ID";
        String X_TOTAL_COUNT = "X-Total-Count";
        String X_TOTAL_PAGES = "X-Total-Pages";

        interface Value {
            String ACCEPT_LINKS_HATEOAS = "HATEOAS";
            String RETURN_MINIMAL = "return=minimal";
            String RETURN_REPRESENTATION = "return=representation";
        }
    }

    String RESERVED_PARAM = "~";

    interface Param {
        String FIELDS = RESERVED_PARAM + "fields";
        String SORT = RESERVED_PARAM + "sort";
        String PAGE_NO = RESERVED_PARAM + "pageNo";
        String PAGE_SIZE = RESERVED_PARAM + "pageSize";
    }

}

