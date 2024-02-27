@Protocol("*get | (UNRESOLVED·(put + cancel) + DONE) + CANCELLED")
public class Future<A> {
    @State private void UNRESOLVED();
    @State private void DONE(A val);
    @State private void CANCELLED();
    @Operation public void put(A val);
    @Operation public void cancel();
    @Operation public A get();
    @Reaction private void when_UNRESOLVED_put(A val) { this.DONE(val); }
    @Reaction private void when_UNRESOLVED_cancel() { this.CANCELLED(); }
    @Reaction private A when_DONE_get(A val) {
	this.DONE(val);
	return val;
    }
    public Future() { this.UNRESOLVED(); }
}
