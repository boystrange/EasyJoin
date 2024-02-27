@Protocol("NONE·hasNext + SOME·(hasNext + next)")
public class ArrayIterator<T> {
    private T[] data;
    private int i;

    @State private void NONE();
    @State private void SOME();

    @Operation  public  boolean hasNext();
    @Operation  public  T next();

    @Reaction private boolean when_NONE_hasNext() {
	this.NONE();
	return false;
    }
    @Reaction private boolean when_SOME_hasNext() {
	this.SOME();
	return true;
    }
    @Reaction private T when_SOME_next() {
	final T val = this.data[i++];
	setState();
	return val;
    }

    private void setState() {
	if (i < data.length) this.SOME(); else this.NONE();
    }

    public ArrayIterator(T[] data) {
	assert data != null;
	this.data = data;
	this.i = 0;
	setState();
    }
}
