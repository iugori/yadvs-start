package ro.iugori.yadvs.util.rest;


public interface RestApi {

    interface URI {

        String ROOT = "/yadvs/rest/v1";

        interface Polls {
            String ID = "/polls";
            String ROOT = URI.ROOT + ID;

            String ACTIVATE = "/activate";
            String SUSPEND = "/suspend";
            String CLOSE = "/close";
            String ARCHIVE = "/archive";
        }

        interface Options {
            String ID = "/options";
        }

        interface Votes {
            String ID = "/votes";
            String ROOT = URI.ROOT + ID;
        }

        interface Reports {
            String ID = "/reports";
            String ROOT = URI.ROOT + ID;

            String POLL = "/poll";
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

    interface MediaType {
        org.springframework.http.MediaType APPLICATION_JSON_PATCH_JSON = new org.springframework.http.MediaType("application", "json-patch+json");
        org.springframework.http.MediaType APPLICATION_MERGE_PATCH_JSON = new org.springframework.http.MediaType("application", "merge-patch+json");
        String APPLICATION_MERGE_PATCH_JSON_VALUE = "application/merge-patch+json";
    }

}
