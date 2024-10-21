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
        String X_TOTAL_COUNT = "X-Total-Count";
        String X_TOTAL_PAGES = "X-Total-Pages";
        String X_CORRELATION_ID = "X-Correlation-ID";

        interface Value {
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

    interface MediaType {
        org.springframework.http.MediaType APPLICATION_JSON_PATCH_JSON = new org.springframework.http.MediaType("application", "json-patch+json");
        org.springframework.http.MediaType APPLICATION_MERGE_PATCH_JSON = new org.springframework.http.MediaType("application", "merge-patch+json");
    }

}
