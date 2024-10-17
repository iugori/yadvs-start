package ro.iugori.yadvs.delegate.refiner;

public record RecordRefiner(Projector projector, Selector selector, Sorter sorter, Integer offset, Integer limit) {

    public static RecordRefinerBuilder builder() {
        return new RecordRefinerBuilder();
    }

    public static int pageFromOffset(int offset, int pageSize) {
        if (offset < 0 || pageSize <= 0) {
            throw new IllegalArgumentException("Offset and pageSize must be non-negative.");
        }
        return offset / pageSize + 1;
    }

    public static int offsetFromPage(int pageNo, int pageSize) {
        if (pageNo <= 0 || pageSize <= 0) {
            throw new IllegalArgumentException("Page and pageSize must be positive.");
        }
        return (pageNo - 1) * pageSize;
    }

    public boolean isEmpty() {
        return (projector == null || projector.isEmpty())
                && (selector == null || selector.isEmpty())
                && (sorter == null || sorter.isEmpty())
                && offset == null
                && limit == null;
    }

}
