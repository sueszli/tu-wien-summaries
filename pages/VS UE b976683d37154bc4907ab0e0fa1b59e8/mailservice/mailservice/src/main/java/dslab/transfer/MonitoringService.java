package dslab.transfer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;

public class MonitoringService {

    private final String monitoringHost;
    private final int monitoringPort;

    public MonitoringService(String monitoringHost, int monitoringPort) {
        this.monitoringHost = monitoringHost;
        this.monitoringPort = monitoringPort;
    }

    public void sendData(String data) {
        try (DatagramSocket monitoringSocket = new DatagramSocket()) {

            byte[] buffer = data.getBytes();

            InetSocketAddress address = new InetSocketAddress(monitoringHost, monitoringPort);

            DatagramPacket packet = new DatagramPacket(buffer, buffer.length, address);

            monitoringSocket.send(packet);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
