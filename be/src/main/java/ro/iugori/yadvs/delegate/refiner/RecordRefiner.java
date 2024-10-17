package ro.iugori.yadvs.delegate.refiner;

public record RecordRefiner(Filter filter, Sorter sorter, int limit, int page) {

    public static RecordRefinerBuilder builder() {
        return new RecordRefinerBuilder();
    }

}
