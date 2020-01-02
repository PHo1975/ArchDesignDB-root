package definition.data

class PlotFilter (val filter:Int) extends AnyVal {
  import PlotFilter._
  def filterLine:Boolean=(filter & filterLine_id)>0
  def filterCicle:Boolean=(filter & filterCircle_id)>0
  def filterElipse:Boolean=(filter & filterElipse_id)>0
  def filterPolygon:Boolean=(filter & filterPolygon_id)>0
  def filterPolygonLine:Boolean=(filter & filterPolygonLine_id)>0
  def filterBitmap:Boolean=(filter & filterBitmap_id)>0
  def filterMeasure:Boolean=(filter & filterMeasure_id)>0
  def filterSymbol:Boolean=(filter & filterSymbol_id)>0
  def filterSymbolFiller:Boolean=(filter & filterSymbolFiller_id)>0
  def filterAreaPolygon:Boolean=(filter & filterAreaPolygon_id)>0
  def filterWohnflache:Boolean=(filter & filterWohnflaeche_id)>0
  def filterMeasurePolyLine:Boolean=(filter & filterMeasurePolyLine_id)>0

  def setFilterLine=new PlotFilter(filter | filterLine_id)
  def setFilterCircle=new PlotFilter(filter | filterCircle_id)
  def setFilterElipse=new PlotFilter(filter | filterElipse_id)
  def setFilterPolygon=new PlotFilter(filter | filterPolygon_id)
  def setFilterPolygonLine=new PlotFilter(filter | filterPolygonLine_id)
  def setFilterBitmap=new PlotFilter(filter | filterBitmap_id)
  def setFilterMeasure=new PlotFilter(filter | filterMeasure_id)
  def setFilterSymbol=new PlotFilter(filter | filterSymbol_id)
  def setFilterSymbolFiller=new PlotFilter(filter | filterSymbolFiller_id)

}

object PlotFilter {
  val filterLine_id=1
  val filterCircle_id=2
  val filterElipse_id=4
  val filterPolygon_id=8
  val filterPolygonLine_id=16
  val filterBitmap_id=32
  val filterText_id=64
  val filterMeasure_id=128
  val filterSymbol_id=256
  val filterSymbolFiller_id=512
  val filterAreaPolygon_id=1024
  val filterWohnflaeche_id=2048
  val filterMeasurePolyLine_id=4096

  val emptyFilter:PlotFilter=new PlotFilter(0)
}
