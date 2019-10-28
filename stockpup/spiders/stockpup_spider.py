from scrapy import Spider, Request
from stockpup.items import StockpupItem
import pandas as pd
import re



class StockpupSpider(Spider):
    name = 'stockpup_spider'
    allowed_domains = ['www.stockpup.com']
    start_urls = ['http://www.stockpup.com/companies/']

    def parse(self, response):
        companies_url = response.xpath("//ul[@class='nav nav-pills nav-stacked']/li/a/@href").extract() 
      
        companies_urls = ['http://www.stockpup.com{}'.format(x) for x in companies_url]

        for url in companies_urls:
            yield Request(url = url, callback = self.parse_result_page)

    def parse_result_page(self, response):
        
        industries = response.xpath('//div[@class="col-xs-9"]/p/a/text()').extract()
        description = response.xpath('//div[@class="col-xs-9"]/p[2]/text()').extract_first()
        company = response.xpath('//h1[@style="display:inline;"]/text()').extract_first()

        item = StockpupItem()
        item['industries'] = industries
        item['description'] = description
        item['company'] = company
        
        yield item

