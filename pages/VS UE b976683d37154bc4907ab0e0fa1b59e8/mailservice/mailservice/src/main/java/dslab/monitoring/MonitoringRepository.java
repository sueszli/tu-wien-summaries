package dslab.monitoring;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicLong;

public class MonitoringRepository {

    private final ConcurrentMap<String, AtomicLong> addresses = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, AtomicLong> servers = new ConcurrentHashMap<>();

    public MonitoringRepository() {
    }

    public void insertAddress(String address) {
        addresses.putIfAbsent(address, new AtomicLong(0));
        addresses.get(address).incrementAndGet();
    }

    public void insertServer(String server) {
        servers.putIfAbsent(server, new AtomicLong(0));
        servers.get(server).incrementAndGet();
    }

    public String getFormattedAddresses() {
        return getFormattedString(addresses);
    }

    public String getFormattedServers() {
        return getFormattedString(servers);
    }


    private String getFormattedString(ConcurrentMap<String, AtomicLong> map) {
        StringBuilder result = new StringBuilder();
        for (Map.Entry<String, AtomicLong> e : map.entrySet()) {
            result.append(e.getKey()).append(" ").append(e.getValue().get()).append("\n");
        }
        return result.toString();
    }
}
