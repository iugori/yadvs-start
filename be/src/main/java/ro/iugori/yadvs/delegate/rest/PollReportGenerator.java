package ro.iugori.yadvs.delegate.rest;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.plot.PiePlot;
import org.jfree.data.general.DefaultPieDataset;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.model.entity.PollOptionEntity;
import ro.iugori.yadvs.model.entity.PollResultEntity;
import ro.iugori.yadvs.model.rest.sturctured.BasicPollReport;

import javax.imageio.ImageIO;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;


public class PollReportGenerator {

    private final PollEntity pollEntity;
    private final List<PollOptionEntity> optionEntities;
    private final List<PollResultEntity> reportEntities;

    public PollReportGenerator(List<PollOptionEntity> optionEntities, List<PollResultEntity> reportEntities) {
        this.optionEntities = optionEntities;
        this.reportEntities = reportEntities;
        this.pollEntity = optionEntities.getFirst().getPoll();
    }

    public BasicPollReport generateBasicReport() {
        var pollReport = new BasicPollReport();
        pollReport.setTitle(pollEntity.getName());
        pollReport.setDescription(pollEntity.getDescription());
        pollReport.setOptions(new ArrayList<>());
        optionEntities.stream()
                .sorted()
                .forEach(optionEntity -> {
                    var entry = new Object[]{optionEntity.getDescription(), 0};
                    reportEntities.stream()
                            .filter(reportEntity -> reportEntity.getOptionId().equals(optionEntity.getId()))
                            .findAny()
                            .ifPresent(reportEntity -> entry[1] = reportEntity.getVoteCount());
                    pollReport.getOptions().add(entry);
                });
        return pollReport;
    }

    public byte[] generatePngReport() throws IOException {
        var basicReport = generateBasicReport();
        var dataset = new DefaultPieDataset<String>();
        basicReport.getOptions().forEach(dataPoint -> {
            dataset.setValue((String) dataPoint[0], (Number) dataPoint[1]);
        });
        var chart = ChartFactory.createRingChart(basicReport.getTitle(), dataset, true, true, Locale.ROOT);
        var plot = (PiePlot<?>) chart.getPlot();
        plot.setSectionOutlinesVisible(false);
        plot.setLabelGenerator(null);
        var image = chart.createBufferedImage(320, 240);
        var baos = new ByteArrayOutputStream();
        ImageIO.write(image, "png", baos);
        return baos.toByteArray();
    }

}
