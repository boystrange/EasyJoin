@Protocol("CLOSED·open + OPEN·(close + read + write)")
public class File {
    @State private void CLOSED();
    @State private void OPEN();
    @Operation public void open();
    @Operation public void close();
    @Operation public void read();
    @Operation public void write();
    @Reaction private void when_CLOSED_open() { this.OPEN(); }
    @Reaction private void when_OPEN_close() { this.CLOSED(); }
    @Reaction private void when_OPEN_read() { this.OPEN(); }
    @Reaction private void when_OPEN_write() { this.OPEN(); }
    public File() { this.CLOSED(); }
}
