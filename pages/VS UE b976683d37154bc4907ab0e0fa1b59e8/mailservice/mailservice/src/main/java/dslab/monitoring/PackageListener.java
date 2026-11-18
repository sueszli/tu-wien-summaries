package dslab.monitoring;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.Arrays;

public class PackageListener extends Thread {

    private final DatagramSocket socket;
    private final int MAX_BUFFER_LENGTH = 1024;
    private final MonitoringRepository repo;
    private boolean shutdown;

    public PackageListener(DatagramSocket socket, MonitoringRepository repo) {
        this.socket = socket;
        this.repo = repo;
    }

    @Override
    public void run() {
        byte[] buffer;
        DatagramPacket packet;

        while (!shutdown) {
            buffer = new byte[MAX_BUFFER_LENGTH];
            packet = new DatagramPacket(buffer, buffer.length);

            try {
                socket.receive(packet);
            } catch (SocketException e) {
                if (shutdown) break;
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }

            storeData(new String(Arrays.copyOfRange(packet.getData(), 0, packet.getLength())));
        }
    }

    public void shutdown() {
        shutdown = true;
    }

    private void storeData(String data) {
        data = data.trim();
        String regularExpr = "^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}):([0-9]{1,5})\\s([a-zA-Z0-9_!#$%&'*+=?`{|}~^.-]+@[a-zA-Z0-9.-]+)$";

        if (!data.matches(regularExpr)) return;
        String[] serverAddress = data.split("\\s");
        if (serverAddress.length != 2) return;

        repo.insertServer(serverAddress[0]);
        repo.insertAddress(serverAddress[1]);
    }


}
